use std::cmp::min;

use crate::*;
use BindingMode::*;
use Mutability::*;

impl<'a> Type<'a> {
    /// Whether the type implements `Copy` (we assume type variables are `Copy`).
    pub fn is_copy(&self) -> bool {
        match self {
            Type::Tuple(tys) => tys.iter().all(|ty| ty.is_copy()),
            Type::Ref(Shared, _) => true,
            Type::Ref(Mutable, _) => false,
            Type::Var(_) => true,
        }
    }
}

#[derive(Debug)]
pub enum BorrowCheckError {
    CantCopyRefMut,
    MutBorrowBehindSharedBorrow,
}

impl<'a> Expression<'a> {
    /// An expression is either a place or a reference to a place. This corresponds to the "default
    /// binding mode" of RFC2005 aka "match ergonomics".
    pub fn binding_mode(&self) -> BindingMode {
        match self.kind {
            ExprKind::Scrutinee | ExprKind::Deref(_) | ExprKind::Field(_, _) => ByMove,
            ExprKind::Ref(mtbl, _) => ByRef(mtbl),
            ExprKind::CastAsImmRef(_) => ByRef(Shared),
        }
    }

    /// Resets the binding mode to `move`.
    pub fn reset_binding_mode(&self) -> Self {
        match self.kind {
            ExprKind::Scrutinee | ExprKind::Deref(_) | ExprKind::Field(_, _) => *self,
            ExprKind::Ref(_, e) => *e,
            ExprKind::CastAsImmRef(e) => e.reset_binding_mode(),
        }
    }

    /// Borrow-check the expression.
    pub fn borrow_check(&self) -> Result<(), BorrowCheckError> {
        self.borrow_check_inner(true)
    }

    fn borrow_check_inner(&self, top_level: bool) -> Result<(), BorrowCheckError> {
        match self.kind {
            ExprKind::Scrutinee => Ok(()),
            ExprKind::Ref(mtbl, e) => {
                if mtbl == Mutable && e.scrutinee_access_level() == ByRef(Shared) {
                    Err(BorrowCheckError::MutBorrowBehindSharedBorrow)
                } else {
                    e.borrow_check_inner(false)
                }
            }
            ExprKind::Deref(e) | ExprKind::Field(e, _) => {
                if top_level && !self.ty.is_copy() && self.scrutinee_access_level() != ByMove {
                    Err(BorrowCheckError::CantCopyRefMut)
                } else {
                    e.borrow_check_inner(false)
                }
            }
            ExprKind::CastAsImmRef(e) => e.borrow_check_inner(false),
        }
    }

    /// Computes what access we have to the scrutinee. By default we have move access, and if we go
    /// under references we get by-reference binding mode of the least permissive reference
    /// encountered.
    pub fn scrutinee_access_level(&self) -> BindingMode {
        match self.kind {
            ExprKind::Scrutinee => ByMove,
            ExprKind::Ref(mtbl, e) => min(ByRef(mtbl), e.scrutinee_access_level()),
            ExprKind::Deref(e) => {
                let bm = if let Type::Ref(mtbl, _) = *e.ty {
                    ByRef(mtbl)
                } else {
                    ByMove
                };
                min(bm, e.scrutinee_access_level())
            }
            ExprKind::Field(e, _) => e.scrutinee_access_level(),
            ExprKind::CastAsImmRef(_) => ByRef(Shared),
        }
    }

    /// Simplify this expression without changing its semantics. In particular, this should not
    /// change borrow-checking behavior.
    pub fn simplify(&self, a: &'a Arenas<'a>) -> Self {
        self.simplify_inner(a, true)
    }

    fn simplify_inner(&self, a: &'a Arenas<'a>, top_level: bool) -> Self {
        match self.kind {
            ExprKind::Scrutinee => *self,
            ExprKind::Ref(mtbl, e) => match e.kind {
                // `&*p` with `p: &T` can be simplified, but `&mut *p` with `p: &mut T` is a
                // re-borrow so it must stay.
                ExprKind::Deref(inner)
                    if mtbl == Shared && matches!(inner.ty, Type::Ref(Shared, _)) =>
                {
                    *inner
                }
                _ => Expression {
                    ty: self.ty,
                    kind: ExprKind::Ref(mtbl, e.simplify_inner(a, false).alloc(a)),
                },
            },
            ExprKind::Deref(e) => match e.kind {
                ExprKind::Ref(mtbl, inner) if inner.scrutinee_access_level() == ByRef(mtbl) => {
                    *inner
                }
                ExprKind::Ref(mtbl, inner) if top_level && mtbl == Shared && self.ty.is_copy() => {
                    *inner
                }
                ExprKind::CastAsImmRef(inner)
                    if inner.scrutinee_access_level() == ByRef(Shared) =>
                {
                    Expression {
                        ty: self.ty,
                        kind: ExprKind::Deref(inner),
                    }
                    .simplify_inner(a, top_level)
                }
                _ => Expression {
                    ty: self.ty,
                    kind: ExprKind::Deref(e.simplify_inner(a, false).alloc(a)),
                },
            },
            ExprKind::Field(e, n) => Expression {
                ty: self.ty,
                kind: ExprKind::Field(e.simplify_inner(a, false).alloc(a), n),
            },
            ExprKind::CastAsImmRef(e) => Expression {
                ty: self.ty,
                kind: ExprKind::CastAsImmRef(e.simplify_inner(a, false).alloc(a)),
            },
        }
    }
}

impl<'a> TypingPredicate<'a> {
    /// Simplify the expression in a semantics-preserving way, see `Expression::simplify`.
    pub fn simplify_expr(&self, a: &'a Arenas<'a>) -> Self {
        Self {
            pat: self.pat,
            expr: self.expr.simplify(a),
        }
    }
}
