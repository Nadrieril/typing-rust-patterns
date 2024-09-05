use std::cmp::min;

use crate::*;
use itertools::Itertools;
use BindingMode::*;
use Mutability::*;

impl<'a> Pattern<'a> {
    pub fn depth(&self) -> usize {
        match self {
            Pattern::Abstract(_) => 0,
            Pattern::Binding(..) => 0,
            Pattern::Tuple(pats) => pats.iter().map(|pat| pat.depth() + 1).max().unwrap_or(0),
            Pattern::Ref(_, pat) => 1 + pat.depth(),
        }
    }

    /// Whether the pattern contains an abstract subpattern.
    pub fn contains_abstract(&self) -> bool {
        match *self {
            Pattern::Tuple(pats) => pats.iter().any(|pat| pat.contains_abstract()),
            Pattern::Ref(_, pat) => pat.contains_abstract(),
            Pattern::Binding(..) => false,
            Pattern::Abstract(_) => true,
        }
    }

    /// Replace the abstract patterns (if any) with the given one.
    pub fn subst(&self, a: &'a Arenas<'a>, replace: Self) -> Self {
        match *self {
            Pattern::Tuple(pats) => {
                let pats = pats.iter().map(|pat| pat.subst(a, replace)).collect_vec();
                Pattern::Tuple(a.pat_arena.alloc_extend(pats))
            }
            Pattern::Ref(mtbl, pat) => Pattern::Ref(mtbl, pat.subst(a, replace).alloc(a)),
            Pattern::Binding(..) => *self,
            Pattern::Abstract(_) => replace,
        }
    }
}

impl<'a> Type<'a> {
    pub fn depth(&self) -> usize {
        match self {
            Type::Abstract(_) => 0,
            Type::NonRef(_) => 0,
            Type::Tuple(tys) => tys.iter().map(|ty| ty.depth() + 1).max().unwrap_or(0),
            Type::Ref(_, ty) => 1 + ty.depth(),
        }
    }

    /// Whether the type implements `Copy` (we assume type variables are `Copy`).
    pub fn is_copy(&self) -> bool {
        match self {
            Type::Tuple(tys) => tys.iter().all(|ty| ty.is_copy()),
            Type::Ref(Shared, _) => true,
            Type::Ref(Mutable, _) => false,
            Type::NonRef(_) | Type::Abstract(_) => true,
        }
    }

    pub fn visit(&self, f: &mut impl FnMut(&Self)) {
        f(self);
        match *self {
            Type::Abstract(_) => {}
            Type::NonRef(_) => {}
            Type::Tuple(tys) => {
                for ty in tys {
                    ty.visit(f);
                }
            }
            Type::Ref(_, ty) => ty.visit(f),
        }
    }

    /// Whether the type contains an abstract subtype.
    pub fn contains_abstract(&self) -> bool {
        match *self {
            Type::Tuple(tys) => tys.iter().any(|ty| ty.contains_abstract()),
            Type::Ref(_, ty) => ty.contains_abstract(),
            Type::NonRef(..) => false,
            Type::Abstract(_) => true,
        }
    }

    /// Replace the abstract types (if any) with the given type.
    pub fn subst(&self, a: &'a Arenas<'a>, replace: Self) -> Self {
        match *self {
            Type::Tuple(tys) => {
                let tys = tys.iter().map(|ty| ty.subst(a, replace)).collect_vec();
                Type::Tuple(a.type_arena.alloc_extend(tys))
            }
            Type::Ref(mtbl, ty) => Type::Ref(mtbl, ty.subst(a, replace).alloc(a)),
            Type::NonRef(_) => *self,
            Type::Abstract(_) => replace,
        }
    }
}

impl<'a> TypingRequest<'a> {
    pub fn depth(&self) -> usize {
        std::cmp::max(self.pat.depth(), self.ty.depth())
    }
}

#[derive(Debug)]
pub enum BorrowCheckError {
    CantCopyRefMut,
    CantCopyNestedRefMut,
    MutBorrowBehindSharedBorrow,
}

impl<'a> Expression<'a> {
    pub fn visit(&self, f: &mut impl FnMut(&Self)) {
        f(self);
        match self.kind {
            ExprKind::Scrutinee => {}
            ExprKind::Abstract { .. } => {}
            ExprKind::Ref(_, e) => e.visit(f),
            ExprKind::Deref(e) => e.visit(f),
            ExprKind::Field(e, _) => e.visit(f),
        }
    }

    /// An expression is either a place or a reference to a place. This corresponds to the "default
    /// binding mode" of RFC2005 aka "match ergonomics".
    pub fn binding_mode(&self) -> Result<BindingMode, TypeError> {
        Ok(match self.kind {
            ExprKind::Scrutinee
            | ExprKind::Deref(_)
            | ExprKind::Field(_, _)
            | ExprKind::Abstract {
                not_a_ref: true, ..
            } => ByMove,
            ExprKind::Abstract {
                not_a_ref: false, ..
            } => match self.ty {
                Type::NonRef(..) | Type::Tuple(..) => ByMove,
                Type::Abstract(_) | Type::Ref(..) => {
                    return Err(TypeError::OverlyGeneral(DeepeningRequest::BindingMode))
                }
            },
            ExprKind::Ref(mtbl, _) => ByRef(mtbl),
        })
    }

    /// Resets the binding mode to `move`.
    pub fn reset_binding_mode(&self) -> Result<Self, TypeError> {
        match self.kind {
            ExprKind::Scrutinee
            | ExprKind::Deref(_)
            | ExprKind::Field(_, _)
            | ExprKind::Abstract {
                not_a_ref: true, ..
            } => Ok(*self),
            ExprKind::Abstract {
                not_a_ref: false, ..
            } => Err(TypeError::OverlyGeneral(DeepeningRequest::BindingMode)),
            ExprKind::Ref(_, e) => Ok(*e),
        }
    }

    /// Borrow-check the expression.
    pub fn borrow_check(&self) -> Result<(), BorrowCheckError> {
        self.borrow_check_inner(true)
    }

    fn borrow_check_inner(&self, top_level: bool) -> Result<(), BorrowCheckError> {
        match self.kind {
            ExprKind::Scrutinee => Ok(()),
            ExprKind::Abstract { .. } => Ok(()),
            ExprKind::Ref(mtbl, e) => {
                if mtbl == Mutable && e.scrutinee_access_mode() == ByRef(Shared) {
                    Err(BorrowCheckError::MutBorrowBehindSharedBorrow)
                } else {
                    e.borrow_check_inner(false)
                }
            }
            ExprKind::Deref(e) | ExprKind::Field(e, _) => {
                if top_level && !self.ty.is_copy() && self.scrutinee_access_mode() != ByMove {
                    // Distinguish these two cases because the nested case is not handled by
                    // `match-ergo-formality`.
                    if let Type::Ref(Mutable, ..) = self.ty {
                        Err(BorrowCheckError::CantCopyRefMut)
                    } else {
                        Err(BorrowCheckError::CantCopyNestedRefMut)
                    }
                } else {
                    e.borrow_check_inner(false)
                }
            }
        }
    }

    /// Computes what access we have to the scrutinee. By default we have move access, and if we go
    /// under references we get by-reference binding mode of the least permissive reference
    /// encountered.
    pub fn scrutinee_access_mode(&self) -> BindingMode {
        match self.kind {
            ExprKind::Scrutinee => ByMove,
            ExprKind::Ref(mtbl, e) => min(ByRef(mtbl), e.scrutinee_access_mode()),
            ExprKind::Deref(e) => {
                let bm = if let Type::Ref(mtbl, _) = *e.ty {
                    ByRef(mtbl)
                } else {
                    ByMove
                };
                min(bm, e.scrutinee_access_mode())
            }
            ExprKind::Field(e, _) => e.scrutinee_access_mode(),
            ExprKind::Abstract {
                scrutinee_mutability: Some(Shared),
                ..
            } => ByRef(Shared),
            // We can't know, and strictly speaking we shouldn't assume, but we choose to lie here.
            ExprKind::Abstract { .. } => ByMove,
        }
    }

    /// Restricted version of `scrutinee_access_mode`, usable for deepening. Only distinguishes
    /// `ByRef(Shared)` from the other two (which is sufficient to implement the rules we need).
    pub fn scrutinee_mutability(&self) -> Result<Mutability, TypeError> {
        Ok(match self.kind {
            ExprKind::Scrutinee => Mutable,
            ExprKind::Ref(mtbl, e) => min(mtbl, e.scrutinee_mutability()?),
            ExprKind::Deref(e) => {
                if let Type::Ref(Shared, _) = *e.ty {
                    Shared
                } else {
                    e.scrutinee_mutability()?
                }
            }
            ExprKind::Field(e, _) => e.scrutinee_mutability()?,
            ExprKind::Abstract {
                scrutinee_mutability: Some(mtbl),
                ..
            } => mtbl,
            // We can't know, and strictly speaking we shouldn't assume, but we choose to lie here.
            ExprKind::Abstract { .. } => {
                return Err(TypeError::OverlyGeneral(
                    DeepeningRequest::ScrutineeMutability,
                ))
            }
        })
    }

    /// Simplify this expression without changing its semantics. In particular, this should not
    /// change borrow-checking behavior.
    pub fn simplify(&self, ctx: TypingCtx<'a>) -> Self {
        self.simplify_inner(ctx, true)
    }

    fn simplify_inner(&self, ctx: TypingCtx<'a>, top_level: bool) -> Self {
        match self.kind {
            ExprKind::Scrutinee | ExprKind::Abstract { .. } => *self,
            ExprKind::Ref(mtbl, e) => {
                let e = e.simplify_inner(ctx, false);
                match e.kind {
                    // `&*p` with `p: &T` can be simplified, but `&mut *p` with `p: &mut T` is a
                    // re-borrow so it must stay.
                    ExprKind::Deref(inner)
                        if mtbl == Shared && matches!(inner.ty, Type::Ref(Shared, _)) =>
                    {
                        *inner
                    }
                    _ => Expression {
                        ty: self.ty,
                        kind: ExprKind::Ref(mtbl, e.alloc(ctx.arenas)),
                    },
                }
            }
            ExprKind::Deref(e) => {
                let e = e.simplify_inner(ctx, false);
                match e.kind {
                    ExprKind::Ref(Shared, inner) if top_level && self.ty.is_copy() => *inner,
                    ExprKind::Ref(Mutable, inner) if ctx.options.simplify_deref_mut => *inner,
                    ExprKind::Ref(mtbl, inner) if inner.scrutinee_access_mode() == ByRef(mtbl) => {
                        *inner
                    }
                    _ => Expression {
                        ty: self.ty,
                        kind: ExprKind::Deref(e.alloc(ctx.arenas)),
                    },
                }
            }
            ExprKind::Field(e, n) => {
                let e = e.simplify_inner(ctx, false);
                Expression {
                    ty: self.ty,
                    kind: ExprKind::Field(e.alloc(ctx.arenas), n),
                }
            }
        }
    }
}

impl<'a> TypingPredicate<'a> {
    /// Simplify the expression in a semantics-preserving way, see `Expression::simplify`.
    pub fn simplify_expr(&self, ctx: TypingCtx<'a>) -> Self {
        Self {
            pat: self.pat,
            expr: self.expr.simplify(ctx),
        }
    }
}
