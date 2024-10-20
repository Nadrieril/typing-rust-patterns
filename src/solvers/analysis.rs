use std::cmp::min;

use crate::*;
use BindingMode::*;
use Mutability::*;

impl<'a> Type<'a> {
    /// Whether the type implements `Copy`.
    pub fn is_copy(&self) -> Result<bool, DeepeningRequest> {
        Ok(match *self {
            Type::Tuple(tys) => {
                for ty in tys {
                    if !ty.is_copy()? {
                        return Ok(false);
                    }
                }
                true
            }
            Type::Ref(Shared, _) => true,
            Type::Ref(Mutable, _) => false,
            Type::OtherNonRef(_) => true,
            Type::AbstractNonRef(_) | Type::Abstract(_) => return Err(DeepeningRequest::Type),
        })
    }

    pub fn visit(&self, f: &mut impl FnMut(&Self)) {
        f(self);
        match *self {
            Type::Abstract(_) => {}
            Type::AbstractNonRef(_) => {}
            Type::OtherNonRef(_) => {}
            Type::Tuple(tys) => {
                for ty in tys {
                    ty.visit(f);
                }
            }
            Type::Ref(_, ty) => ty.visit(f),
        }
    }
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
}

#[derive(Debug, Clone, Copy)]
pub enum BorrowCheckError {
    CantCopyRefMut,
    CantCopyNestedRefMut,
    MutBorrowBehindSharedBorrow,
    OverlyGeneral(DeepeningRequest),
}

impl BindingMode {
    /// Returns the binding mode with least access to the scrutinee: ref < ref mut < move.
    fn least_access(self, other: Self) -> Self {
        match (self, other) {
            (ByRef(s), ByRef(o)) => ByRef(min(s, o)),
            (_, ByMove) => self,
            (ByMove, _) => other,
        }
    }
}

impl<'a> Expression<'a> {
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
                Type::OtherNonRef(_) | Type::AbstractNonRef(..) | Type::Tuple(..) => ByMove,
                Type::Abstract(_) | Type::Ref(..) => {
                    return Err(TypeError::TooAbstract(DeepeningRequest::BindingMode))
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
            } => Err(TypeError::TooAbstract(DeepeningRequest::BindingMode)),
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
                if mtbl == Mutable && e.scrutinee_access_mode()? == ByRef(Shared) {
                    Err(BorrowCheckError::MutBorrowBehindSharedBorrow)
                } else {
                    e.borrow_check_inner(false)
                }
            }
            ExprKind::Deref(e) | ExprKind::Field(e, _) => {
                if top_level && self.scrutinee_access_mode()? != ByMove && !self.ty.is_copy()? {
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
    pub fn scrutinee_access_mode(&self) -> Result<BindingMode, DeepeningRequest> {
        Ok(match self.kind {
            ExprKind::Scrutinee => ByMove,
            ExprKind::Ref(mtbl, e) => ByRef(mtbl).least_access(e.scrutinee_access_mode()?),
            ExprKind::Deref(e) => {
                let bm = if let Type::Ref(mtbl, _) = *e.ty {
                    ByRef(mtbl)
                } else {
                    ByMove
                };
                bm.least_access(e.scrutinee_access_mode()?)
            }
            ExprKind::Field(e, _) => e.scrutinee_access_mode()?,
            ExprKind::Abstract {
                scrutinee_mutability: Some(mtbl),
                ..
            } => ByRef(mtbl),
            ExprKind::Abstract { .. } => return Err(DeepeningRequest::ScrutineeMutability),
        })
    }

    /// Restricted version of `scrutinee_access_mode`, usable for deepening. Only distinguishes
    /// `ByRef(Shared)` from the other two (which is sufficient to implement the rules we need).
    pub fn scrutinee_mutability(&self) -> Result<Mutability, DeepeningRequest> {
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
            ExprKind::Abstract { .. } => return Err(DeepeningRequest::ScrutineeMutability),
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
                    ExprKind::Ref(Shared, inner) if top_level && self.ty.is_copy() == Ok(true) => {
                        *inner
                    }
                    ExprKind::Ref(Mutable, inner) if ctx.options.simplify_deref_mut => *inner,
                    ExprKind::Ref(mtbl, inner)
                        if inner.scrutinee_access_mode() == Ok(ByRef(mtbl)) =>
                    {
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
