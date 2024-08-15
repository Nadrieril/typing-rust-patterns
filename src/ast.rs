//! Representations of patterns, expressions and types.
//!
//! Note: we arena-allocate everything to make pattern-matching easy.

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mutable {
    No,
    Yes,
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum BindingMode {
    ByMove,
    ByRef(Mutable),
}

/// A pattern.
#[derive(Clone, Copy)]
pub enum Pattern<'a> {
    /// The constructor for a type. Our only type is the tuple, represented as `[T, U]`, with its
    /// constructor `[p, q]`.
    Tuple(&'a [Self]),
    /// `&p` or `&mut p`.
    Ref(Mutable, &'a Self),
    /// Bindings: `mut x`, `ref mut x`, etc. We allow things like `mut ref mut x` that aren't
    /// representable in today's rust.
    Binding(Mutable, BindingMode, &'a str),
}

/// A type.
#[derive(Clone, Copy)]
pub enum Type<'a> {
    /// Our only type is the tuple, represented as `[T, U]`, with its constructor `[p, q]`.
    Tuple(&'a [Self]),
    /// Reference type.
    Ref(Mutable, &'a Self),
    /// Type variable, representing an unknown and irrelevant type.
    Var(&'a str),
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
    Ref(Mutable, &'a Expression<'a>),
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

    pub fn borrow(&self, arenas: &'a Arenas<'a>, mtbl: Mutable) -> Self {
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
            ty: Type::Ref(Mutable::No, ty).alloc(arenas),
            kind: ExprKind::CastAsImmRef(self.alloc(arenas)),
        }
    }

    /// An expression is either a place or a reference to a place. This corresponds to the "default
    /// binding mode" of RFC2005 aka "match ergonomics".
    pub fn binding_mode(&self) -> BindingMode {
        match self.kind {
            ExprKind::Scrutinee | ExprKind::Deref(_) | ExprKind::Field(_, _) => BindingMode::ByMove,
            ExprKind::Ref(mtbl, _) => BindingMode::ByRef(mtbl),
            ExprKind::CastAsImmRef(_) => BindingMode::ByRef(Mutable::No),
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
