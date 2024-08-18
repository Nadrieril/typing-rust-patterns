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
    pub fn binding_mode(&self) -> Result<BindingMode, TypeError> {
        Ok(match self.kind {
            ExprKind::Scrutinee
            | ExprKind::Deref(_)
            | ExprKind::Field(_, _)
            | ExprKind::Abstract { not_a_ref: true } => ByMove,
            ExprKind::Abstract { not_a_ref: false } => return Err(TypeError::OverlyGeneralExpr),
            ExprKind::Ref(mtbl, _) => ByRef(mtbl),
        })
    }

    /// Resets the binding mode to `move`.
    pub fn reset_binding_mode(&self) -> Result<Self, TypeError> {
        match self.kind {
            ExprKind::Scrutinee
            | ExprKind::Deref(_)
            | ExprKind::Field(_, _)
            | ExprKind::Abstract { not_a_ref: true } => Ok(*self),
            ExprKind::Abstract { not_a_ref: false } => Err(TypeError::OverlyGeneralExpr),
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
            // We can't know, and strictly speaking we shouldn't assume, but we choose to lie here.
            ExprKind::Abstract { .. } => ByMove,
        }
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
                    ExprKind::Ref(mtbl, inner) if inner.scrutinee_access_level() == ByRef(mtbl) => {
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
