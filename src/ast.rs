//! Representations of patterns, expressions and types.
//!
//! Note: we arena-allocate everything to make pattern-matching easy.

use BindingMode::*;
use Mutability::*;

use crate::TypeError;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Mutability {
    Shared,
    Mutable,
}

impl Mutability {
    pub const ALL: [Self; 2] = [Shared, Mutable];
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BindingMode {
    ByRef(Mutability),
    ByMove,
}

impl BindingMode {
    pub const ALL: [Self; 3] = [ByMove, ByRef(Mutable), ByRef(Shared)];
}

/// A pattern.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Pattern<'a> {
    /// The constructor for a type. Our only type is the tuple, represented as `[T, U]`, with its
    /// constructor `[p, q]`.
    Tuple(&'a [Self]),
    /// `&p` or `&mut p`.
    Ref(Mutability, &'a Self),
    /// Bindings: `mut x`, `ref mut x`, etc. We allow things like `mut ref mut x` that aren't
    /// representable in today's rust.
    Binding(Mutability, BindingMode, &'a str),
    /// An abstract pattern, meant as a placeholder for some unknown pattern. Used only when
    /// exploring possible rules.
    Abstract(&'a str),
}

/// A type.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type<'a> {
    /// An abstract type, meant as a placeholder for some unknown type. Used only when exploring
    /// possible rules.
    Abstract(&'a str),
    /// A variable of some non-reference type. This is what `T` parses to.
    NonRef(&'a str),
    /// Our only type is the tuple, represented as `[T, U]`, with its constructor `[p, q]`.
    Tuple(&'a [Self]),
    /// Reference type.
    Ref(Mutability, &'a Self),
}

impl<'a> Type<'a> {
    pub fn borrow(&'a self, mtbl: Mutability) -> Self {
        Type::Ref(mtbl, self)
    }

    pub fn deref(&self) -> &'a Self {
        let Type::Ref(_, ty) = self else {
            panic!("type error")
        };
        ty
    }
}

/// A scrutinee expression. As we type-check a pattern, we also construct a scrutinee expression
/// that points to the place currently being matched on. At the end, each binding is given assigned
/// to such an expression.
/// We remember the types of inner expressions for convenience.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Expression<'a> {
    pub ty: &'a Type<'a>,
    pub kind: ExprKind<'a>,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ExprKind<'a> {
    /// The starting expression, written `p`.
    Scrutinee,
    /// Reference: `&e` or `&mut e`.
    Ref(Mutability, &'a Expression<'a>),
    /// Reference dereference.
    Deref(&'a Expression<'a>),
    /// Field access.
    Field(&'a Expression<'a>, usize),
    /// An abstract expression, meant as a placeholder for some unknown expression. Used only when
    /// exploring possible rules.
    Abstract {
        /// If true, this is a placeholder for any non-`Ref` expression. If it is false, this
        /// stands for any expression.
        not_a_ref: bool,
        /// If `Some`, we know that we have the given access level to the scrutinee.
        scrutinee_mutability: Option<Mutability>,
    },
}

impl<'a> Expression<'a> {
    pub fn deref(&self, arenas: &'a Arenas<'a>) -> Self {
        Expression {
            ty: self.ty.deref(),
            kind: ExprKind::Deref(self.alloc(arenas)),
        }
    }

    /// Borrow the expression.
    pub fn borrow(&self, arenas: &'a Arenas<'a>, mtbl: Mutability) -> Self {
        Expression {
            ty: self.ty.borrow(mtbl).alloc(arenas),
            kind: ExprKind::Ref(mtbl, self.alloc(arenas)),
        }
    }

    /// Borrow the expression. If `cap_mutability` is set, we will downgrade `&mut` to `&` if we
    /// only have shared access to the scrutinee.
    pub fn borrow_cap_mutability(
        &self,
        arenas: &'a Arenas<'a>,
        mut mtbl: Mutability,
        cap_mutability: bool,
    ) -> Result<Self, TypeError> {
        if cap_mutability && mtbl == Mutable {
            mtbl = self.scrutinee_mutability()?;
        }
        Ok(self.borrow(arenas, mtbl))
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
}

impl ExprKind<'_> {
    pub fn new_abstract() -> Self {
        ExprKind::Abstract {
            not_a_ref: false,
            scrutinee_mutability: None,
        }
    }
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
