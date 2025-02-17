//! Representations of patterns, expressions and types.
//!
//! Note: we arena-allocate everything to make pattern-matching easy.

use std::marker::PhantomData;

use BindingMode::*;
use Mutability::*;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
// Warning: the `Ord` impl is relied on for correctness.
pub enum Mutability {
    Shared,
    Mutable,
}

impl Mutability {
    pub const ALL: [Self; 2] = [Shared, Mutable];
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum BindingMode {
    ByMove,
    ByRef(Mutability),
}

impl BindingMode {
    pub const ALL: [Self; 3] = [ByMove, ByRef(Mutable), ByRef(Shared)];
}

/// A pattern.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Pattern<'a> {
    /// An abstract pattern, meant as a placeholder for some unknown pattern. Used only when
    /// exploring possible rules.
    Abstract(&'a str),
    /// Bindings: `mut x`, `ref mut x`, etc. We allow things like `mut ref mut x` that aren't
    /// representable in today's rust.
    Binding(Mutability, BindingMode, &'a str),
    /// `&p` or `&mut p`.
    Ref(Mutability, &'a Self),
    /// The constructor for a type. Our only type is the tuple, represented as `[T, U]`, with its
    /// constructor `[p, q]`.
    Tuple(&'a [Self]),
}

/// A type.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Type<'a> {
    /// An abstract type, meant as a placeholder for some unknown type. Used only when exploring
    /// possible rules.
    Abstract(&'a str),
    /// An abstract type known not to be a reference.
    AbstractNonRef(&'a str),
    /// A variable of some non-tuple non-reference type. This is what `T` parses to. This type is
    /// assumed to be `Copy`.
    OtherNonRef(&'a str),
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

impl Pattern<'_> {
    pub const ABSTRACT: Self = Pattern::Abstract("p");
}
impl Type<'_> {
    pub const ABSTRACT: Self = Type::Abstract("T");
}
impl ExprKind<'_> {
    pub const ABSTRACT: Self = ExprKind::Abstract {
        not_a_ref: false,
        scrutinee_mutability: None,
    };
}
impl Expression<'_> {
    pub const ABSTRACT: Self = Expression {
        kind: ExprKind::ABSTRACT,
        ty: &Type::ABSTRACT,
    };
}
impl TypingRequest<'_> {
    pub const ABSTRACT: Self = TypingRequest {
        pat: &Pattern::ABSTRACT,
        ty: &Type::ABSTRACT,
    };
}
impl TypingPredicate<'_> {
    pub const ABSTRACT: Self = TypingPredicate {
        pat: &Pattern::ABSTRACT,
        expr: Expression::ABSTRACT,
    };
}

#[derive(Default)]
pub struct Arenas<'a> {
    pub bump: bumpalo::Bump,
    // To avoid having to remove the lifetime param everywhere.
    phantom: PhantomData<&'a ()>,
}

impl<'a> Arenas<'a> {
    pub fn alloc_str(&'a self, s: &str) -> &'a str {
        self.bump.alloc_str(s)
    }
}
impl<'a> Pattern<'a> {
    pub fn alloc(self, arenas: &'a Arenas<'a>) -> &'a Self {
        arenas.bump.alloc(self)
    }
}
impl<'a> Type<'a> {
    pub fn alloc(self, arenas: &'a Arenas<'a>) -> &'a Self {
        arenas.bump.alloc(self)
    }
}
impl<'a> Expression<'a> {
    pub fn alloc(self, arenas: &'a Arenas<'a>) -> &'a Self {
        arenas.bump.alloc(self)
    }
}

/// The input to our solver: the question of whether `pat` can be used at type `ty`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct TypingRequest<'a> {
    pub pat: &'a Pattern<'a>,
    pub ty: &'a Type<'a>,
}

/// The inner state of our solver: the typing of `let pat: type = expr`. We write it `pat @ expr :
/// type`.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypingPredicate<'a> {
    pub pat: &'a Pattern<'a>,
    pub expr: Expression<'a>,
}

impl<'a> TypingPredicate<'a> {
    pub fn new(req: TypingRequest<'a>) -> Self {
        TypingPredicate {
            pat: req.pat,
            expr: Expression {
                kind: ExprKind::Scrutinee,
                ty: req.ty,
            },
        }
    }
}
