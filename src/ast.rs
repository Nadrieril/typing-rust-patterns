//! Representations of patterns, expressions and types.
//!
//! Note: we arena-allocate everything to make pattern-matching easy.

use std::cmp::min;

use BindingMode::*;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mutability {
    Shared,
    Mutable,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BindingMode {
    ByRef(Mutability),
    ByMove,
}

/// A pattern.
#[derive(Clone, Copy)]
pub enum Pattern<'a> {
    /// The constructor for a type. Our only type is the tuple, represented as `[T, U]`, with its
    /// constructor `[p, q]`.
    Tuple(&'a [Self]),
    /// `&p` or `&mut p`.
    Ref(Mutability, &'a Self),
    /// Bindings: `mut x`, `ref mut x`, etc. We allow things like `mut ref mut x` that aren't
    /// representable in today's rust.
    Binding(Mutability, BindingMode, &'a str),
}

/// A type.
#[derive(Clone, Copy)]
pub enum Type<'a> {
    /// Our only type is the tuple, represented as `[T, U]`, with its constructor `[p, q]`.
    Tuple(&'a [Self]),
    /// Reference type.
    Ref(Mutability, &'a Self),
    /// Type variable, representing an unknown and irrelevant type.
    Var(&'a str),
}

impl<'a> Type<'a> {
    /// Whether the type implements `Copy` (we assume type variables are `Copy`).
    pub fn is_copy(&self) -> bool {
        match self {
            Type::Tuple(tys) => tys.iter().all(|ty| ty.is_copy()),
            Type::Ref(Mutability::Shared, _) => true,
            Type::Ref(Mutability::Mutable, _) => false,
            Type::Var(_) => true,
        }
    }
}

/// A scrutinee expression. As we type-check a pattern, we also construct a scrutinee expression
/// that points to the place currently being matched on. At the end, each binding is given assigned
/// to such an expression.
/// We remember the types of inner expressions for convenience.
#[derive(Clone, Copy)]
pub struct Expression<'a> {
    pub ty: &'a Type<'a>,
    pub kind: ExprKind<'a>,
}

#[derive(Clone, Copy)]
pub enum ExprKind<'a> {
    /// The starting expression, written `p`.
    Scrutinee,
    /// Reference: `&e` or `&mut e`.
    Ref(Mutability, &'a Expression<'a>),
    /// Reference dereference.
    Deref(&'a Expression<'a>),
    /// Field access.
    Field(&'a Expression<'a>, usize),
    /// A `(e as &_)` cast.
    CastAsImmRef(&'a Expression<'a>),
}

impl<'a> Expression<'a> {
    pub fn deref(&self, arenas: &'a Arenas<'a>) -> Self {
        let Type::Ref(_, ty) = self.ty else {
            panic!("type error")
        };
        Expression {
            ty,
            kind: ExprKind::Deref(self.alloc(arenas)),
        }
    }

    /// Borrow the expression. If `cap_mutability` is set, we will downgrade `&mut` to `&` if we
    /// only have shared access to the scrutinee.
    pub fn borrow(
        &self,
        arenas: &'a Arenas<'a>,
        mut mtbl: Mutability,
        cap_mutability: bool,
    ) -> Self {
        if cap_mutability && let ByRef(cap) = self.scrutinee_access_level() {
            mtbl = min(mtbl, cap);
        }
        Expression {
            ty: Type::Ref(mtbl, self.ty).alloc(arenas),
            kind: ExprKind::Ref(mtbl, self.alloc(arenas)),
        }
    }

    pub fn field(&self, arenas: &'a Arenas<'a>, n: usize) -> Self {
        let Type::Tuple(tys) = self.ty else {
            panic!("type error")
        };
        Expression {
            ty: &tys[n],
            kind: ExprKind::Field(self.alloc(arenas), n),
        }
    }

    pub fn cast_as_imm_ref(&self, arenas: &'a Arenas<'a>) -> Self {
        let Type::Ref(_, ty) = self.ty else {
            panic!("type error")
        };
        Expression {
            ty: Type::Ref(Mutability::Shared, ty).alloc(arenas),
            kind: ExprKind::CastAsImmRef(self.alloc(arenas)),
        }
    }

    /// An expression is either a place or a reference to a place. This corresponds to the "default
    /// binding mode" of RFC2005 aka "match ergonomics".
    pub fn binding_mode(&self) -> BindingMode {
        match self.kind {
            ExprKind::Scrutinee | ExprKind::Deref(_) | ExprKind::Field(_, _) => ByMove,
            ExprKind::Ref(mtbl, _) => ByRef(mtbl),
            ExprKind::CastAsImmRef(_) => ByRef(Mutability::Shared),
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
                if mtbl == Mutability::Mutable
                    && e.scrutinee_access_level() == ByRef(Mutability::Shared)
                {
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
        use Mutability::*;
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
                    if mtbl == Mutability::Shared
                        && matches!(inner.ty, Type::Ref(Mutability::Shared, _)) =>
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
                ExprKind::Ref(mtbl, inner)
                    if top_level && mtbl == Mutability::Shared && self.ty.is_copy() =>
                {
                    *inner
                }
                ExprKind::CastAsImmRef(inner)
                    if inner.scrutinee_access_level() == ByRef(Mutability::Shared) =>
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

#[derive(Debug)]
pub enum BorrowCheckError {
    CantCopyRefMut,
    MutBorrowBehindSharedBorrow,
}

#[derive(Default)]
pub struct Arenas<'a> {
    pub str_arena: typed_arena::Arena<u8>,
    pub pat_arena: typed_arena::Arena<Pattern<'a>>,
    pub type_arena: typed_arena::Arena<Type<'a>>,
    pub expr_arena: typed_arena::Arena<Expression<'a>>,
}

impl<'a> Pattern<'a> {
    pub fn alloc(self, arenas: &'a Arenas<'a>) -> &'a Self {
        arenas.pat_arena.alloc(self)
    }
}
impl<'a> Type<'a> {
    pub fn alloc(self, arenas: &'a Arenas<'a>) -> &'a Self {
        arenas.type_arena.alloc(self)
    }
}
impl<'a> Expression<'a> {
    pub fn alloc(self, arenas: &'a Arenas<'a>) -> &'a Self {
        arenas.expr_arena.alloc(self)
    }
}

/// The input to our solver: the question of whether `pat` can be used at type `ty`.
#[derive(Clone, Copy)]
pub struct TypingRequest<'a> {
    pub pat: &'a Pattern<'a>,
    pub ty: &'a Type<'a>,
}
