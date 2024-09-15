use itertools::Itertools;

use crate::*;

impl<'a> Pattern<'a> {
    /// Replace the abstract patterns (if any) with the given one.
    pub fn subst(&self, a: &'a Arenas<'a>, replace: Self) -> Self {
        match *self {
            Pattern::Tuple(pats) => {
                let pats = pats.iter().map(|pat| pat.subst(a, replace)).collect_vec();
                Pattern::Tuple(a.bump.alloc_slice_fill_iter(pats))
            }
            Pattern::Ref(mtbl, pat) => Pattern::Ref(mtbl, pat.subst(a, replace).alloc(a)),
            Pattern::Binding(..) => *self,
            Pattern::Abstract(_) => replace,
        }
    }
}

impl<'a> Type<'a> {
    /// Replace the abstract types (if any) with the given type.
    pub fn subst(&self, a: &'a Arenas<'a>, replace: Self) -> Self {
        match *self {
            Type::Tuple(tys) => {
                let tys = tys.iter().map(|ty| ty.subst(a, replace)).collect_vec();
                Type::Tuple(a.bump.alloc_slice_fill_iter(tys))
            }
            Type::Ref(mtbl, ty) => Type::Ref(mtbl, ty.subst(a, replace).alloc(a)),
            Type::OtherNonRef(_) => *self,
            Type::AbstractNonRef(_) => panic!("trying to substitute into `AbstractNonRef`"),
            Type::Abstract(_) => replace,
        }
    }
}

impl<'a> Expression<'a> {
    /// Replace the abstract types (if any) with the given type.
    pub fn subst_ty(&self, a: &'a Arenas<'a>, replace: Type<'a>) -> Self {
        match self.kind {
            ExprKind::Scrutinee | ExprKind::ABSTRACT => Expression {
                ty: self.ty.subst(a, replace).alloc(a),
                kind: self.kind,
            },
            ExprKind::Abstract { .. } => {
                panic!("Can't substitute the type of a partially-abstract expression")
            }
            ExprKind::Ref(mtbl, e) => e.subst_ty(a, replace).borrow(a, mtbl),
            ExprKind::Deref(e) => e.subst_ty(a, replace).deref(a),
            ExprKind::Field(e, n) => e.subst_ty(a, replace).field(a, n),
        }
    }
}

impl<'a> TypingRequest<'a> {
    /// Replace the abstract patterns (if any) with the given type.
    pub fn subst_pat(&self, a: &'a Arenas<'a>, replace: Pattern<'a>) -> Self {
        Self {
            pat: self.pat.subst(a, replace).alloc(a),
            ty: self.ty,
        }
    }

    /// Replace the abstract types (if any) with the given type.
    pub fn subst_ty(&self, a: &'a Arenas<'a>, replace: Type<'a>) -> Self {
        Self {
            pat: self.pat,
            ty: self.ty.subst(a, replace).alloc(a),
        }
    }
}

impl<'a> TypingPredicate<'a> {
    /// Replace the abstract patterns (if any) with the given type.
    pub fn subst_pat(&self, a: &'a Arenas<'a>, replace: Pattern<'a>) -> Self {
        Self {
            pat: self.pat.subst(a, replace).alloc(a),
            expr: self.expr,
        }
    }

    /// Replace the abstract types (if any) with the given type.
    pub fn subst_ty(&self, a: &'a Arenas<'a>, replace: Type<'a>) -> Self {
        Self {
            pat: self.pat,
            expr: self.expr.subst_ty(a, replace),
        }
    }
}

impl<'a> TypingResult<'a> {
    /// Replace the abstract types (if any) with the given type.
    pub fn subst_ty(&self, a: &'a Arenas<'a>, replace: Type<'a>) -> Self {
        match *self {
            TypingResult::Success(ty) => TypingResult::Success(ty.subst(a, replace)),
            TypingResult::BorrowError(ty, e) => TypingResult::BorrowError(ty.subst(a, replace), e),
            TypingResult::TypeError(_) => *self,
        }
    }
}
