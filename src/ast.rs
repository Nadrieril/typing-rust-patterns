//! Representations of patterns, expressions and types.
//!
//! Note that we arena-allocate everything to make pattern-matching easy.

#[derive(Clone, Copy)]
pub enum Mutable {
    Yes,
    No,
}

#[derive(Clone, Copy)]
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

/// The input to our solver: the question of whether `pat` can be used at type `ty`.
#[derive(Clone, Copy)]
pub struct TypingRequest<'a> {
    pub pat: &'a Pattern<'a>,
    pub ty: &'a Type<'a>,
}
