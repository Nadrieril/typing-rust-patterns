//! Representations of patterns, expressions and types.
//!
//! Note: we arena-allocate everything to make pattern-matching easy.

use std::cmp::min;

use BindingMode::*;
use Mutability::*;

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mutability {
    Shared,
    Mutable,
}

impl Mutability {
    pub const ALL: [Self; 2] = [Shared, Mutable];
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BindingMode {
    ByRef(Mutability),
    ByMove,
}

impl BindingMode {
    pub const ALL: [Self; 3] = [ByMove, ByRef(Mutable), ByRef(Shared)];
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
    /// An abstract pattern, meant as a placeholder for some unknown pattern. Used only when
    /// exploring possible rules.
    Abstract(&'a str),
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
    /// An abstract expression, meant as a placeholder for some unknown expression. Used only when
    /// exploring possible rules. The binding mode may be known or unknown. If `not_a_ref` is true,
    /// this is a placeholder for any non-`Ref` expression. If it is false, this stands for any
    /// expression at all.
    Abstract { not_a_ref: bool },
}

impl<'a> Expression<'a> {
    pub fn deref(&self, arenas: &'a Arenas<'a>) -> Self {
        Expression {
            ty: self.ty.deref(),
            kind: ExprKind::Deref(self.alloc(arenas)),
        }
    }

    /// Dereference, or reset the binding mode if it was mutable (this strictly removes borrow
    /// errors).
    pub fn deref_or_reset(&self, a: &'a Arenas<'a>) -> Self {
        if self.binding_mode().is_ok_and(|bm| bm == ByRef(Mutable)) {
            self.reset_binding_mode().unwrap()
        } else {
            self.deref(a)
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
    ) -> Self {
        if cap_mutability && let ByRef(cap) = self.scrutinee_access_level() {
            mtbl = min(mtbl, cap);
        }
        self.borrow(arenas, mtbl)
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
