use serde::{Deserialize, Serialize};
use std::cmp::min;

use crate::*;

/// What to do to a `ref x` binding to an `&p` or `&mut p` expression (as opposed to an inner place
/// of the scrutinee).
#[derive(Debug, Clone, Copy, Hash, Serialize, Deserialize)]
pub enum RefOnRefBehavior {
    /// Borrow that expression, which requires allocating a temporary variable.
    AllocTemporary,
    /// Stable rust behavior: skip the borrow in the expression and re-borrow the inner.
    Skip,
    /// Treat this as an error.
    Error,
}

/// What to do to a `mut x` binding to an `&p` or `&mut p` expression (as opposed to an inner place
/// of the scrutinee).
#[derive(Debug, Clone, Copy, Hash, Serialize, Deserialize)]
pub enum MutOnRefBehavior {
    /// Stable rust behavior: reset the binding mode.
    ResetBindingMode,
    /// Declare the expected binding and make it mutable.
    Keep,
    /// Treat this as an error.
    Error,
}

/// Choice of typing rules.
#[derive(Debug, Clone, Copy, Hash, Serialize, Deserialize)]
pub struct RuleOptions {
    pub ref_on_ref: RefOnRefBehavior,
    pub mut_on_ref: MutOnRefBehavior,
    /// Whether a `&p` pattern is allowed on `&mut T`.
    pub allow_ref_pat_on_ref_mut: bool,
    /// Whether to simplify some expressions, which removes some borrow errors involving mixes of
    /// `&mut` and `&`.
    pub simplify_expressions: bool,
    /// Stable rust behavior: when a `&p` pattern applies to `&&T` where the outer `&` is an
    /// inherited reference, the `&` pattern consumes both layers of reference type.
    pub eat_two_layers: bool,
    /// If false, a `&p` pattern is not allowed on a `&T` if the reference is inherited (except in
    /// the case above i.e. if `T` itself it some `&U`).
    pub eat_inherited_ref_alone: bool,
    /// If we've dereferenced a shared reference, any subsequent `&mut` inherited reference becomes
    /// `&`. This is RFC3627 rule 3.
    pub downgrade_shared_inside_shared: bool,
}

impl RuleOptions {
    /// Reproduces stable rust behavior.
    pub const STABLE_RUST: Self = RuleOptions {
        ref_on_ref: RefOnRefBehavior::Skip,
        mut_on_ref: MutOnRefBehavior::ResetBindingMode,
        allow_ref_pat_on_ref_mut: false,
        simplify_expressions: false,
        eat_two_layers: true,
        eat_inherited_ref_alone: false,
        downgrade_shared_inside_shared: false,
    };

    /// Reproduces RFC3627 (match ergonomics 2024) behavior
    // TODO:
    // Rule 2: When a reference pattern matches against a reference, do not update the DBM.
    // Rule 4:
    //     if (pat, ty) =
    //       (&p, &T) => no
    //       (&p, &mut T) => no
    //       (&p, T) => yes
    //       (&mut p, &T) => yes
    //       (&mut p, &mut T) => no
    //       (&mut p, T) => yes
    //       _ => no
    //     and if the DBM is ref or ref mut, match the pattern against the DBM as though it were a type.
    pub const ERGO2024: Self = RuleOptions {
        ref_on_ref: RefOnRefBehavior::Skip,
        mut_on_ref: MutOnRefBehavior::Error,
        allow_ref_pat_on_ref_mut: true,
        simplify_expressions: true,
        eat_two_layers: false,
        eat_inherited_ref_alone: true,
        downgrade_shared_inside_shared: true,
    };

    /// A fairly permissive proposal.
    pub const PERMISSIVE: Self = RuleOptions {
        ref_on_ref: RefOnRefBehavior::AllocTemporary,
        mut_on_ref: MutOnRefBehavior::Keep,
        allow_ref_pat_on_ref_mut: true,
        simplify_expressions: true,
        eat_two_layers: false,
        eat_inherited_ref_alone: true,
        downgrade_shared_inside_shared: false,
    };

    /// A fairly permissive proposal, with the benefit of requiring 0 implicit state: we never
    /// inspect the DBM, we only follow the types.
    pub const STATELESS: Self = RuleOptions {
        simplify_expressions: false,
        ..Self::PERMISSIVE
    };

    /// My favored proposal.
    pub const NADRIS_PROPOSAL: Self = RuleOptions {
        ref_on_ref: RefOnRefBehavior::Error,
        ..Self::PERMISSIVE
    };
}

/// The various typing rules we can apply.
#[derive(Debug, Clone, Copy)]
pub enum Rule {
    Constructor,
    ConstructorRef,
    ConstructorMultiRef,
    Deref,
    Binding,
    ExprSimplification,
}

#[derive(Debug, Clone, Copy)]
pub enum TypeError {
    TypeMismatch,
    MutabilityMismatch,
    RefOnRef,
    MutOnRef,
}

impl<'a> TypingPredicate<'a> {
    /// Apply one step of rule to this predicate.
    pub fn step(&self, ctx: TypingCtx<'a>) -> Result<(Rule, Vec<Self>), TypeError> {
        use crate::Mutable::*;
        use BindingMode::*;
        use ExprKind as E;
        use Pattern as P;
        use Type as T;
        let a = ctx.arenas;

        if ctx.options.simplify_expressions {
            // Expression simplification rules.
            match self.expr.kind {
                E::CastAsImmRef(Expression {
                    kind: E::Ref(Mutable, e),
                    ..
                }) => {
                    return Ok((
                        Rule::ExprSimplification,
                        vec![Self {
                            pat: self.pat,
                            expr: e.borrow(a, Shared),
                        }],
                    ))
                }
                E::Deref(Expression {
                    kind: E::Ref(Mutable, &e),
                    ..
                }) => {
                    return Ok((
                        Rule::ExprSimplification,
                        vec![Self {
                            pat: self.pat,
                            expr: e,
                        }],
                    ))
                }
                _ => {}
            }
        }

        let bm = self.expr.binding_mode();
        let type_of_underlying_place = self.expr.reset_binding_mode().ty;
        match (*self.pat, *self.expr.ty) {
            // Constructor rules
            (P::Tuple(pats), T::Tuple(tys)) if pats.len() == tys.len() => {
                let preds = pats
                    .iter()
                    .enumerate()
                    .map(|(i, pat)| {
                        let expr = self.expr.field(a, i);
                        Self { pat, expr }
                    })
                    .collect();
                Ok((Rule::Constructor, preds))
            }
            (P::Tuple(pats), T::Ref(mtbl, T::Tuple(tys))) if pats.len() == tys.len() => {
                let preds = pats
                    .iter()
                    .enumerate()
                    .map(|(i, pat)| {
                        let mut mtbl = mtbl;
                        if ctx.options.downgrade_shared_inside_shared {
                            mtbl = min(mtbl, self.expr.scrutinee_access_level());
                        }
                        let expr = self.expr.deref(a).field(a, i).borrow(a, mtbl);
                        Self { pat, expr }
                    })
                    .collect();
                Ok((Rule::ConstructorRef, preds))
            }
            (P::Tuple(_), T::Ref(outer_mtbl, &T::Ref(inner_mtbl, _))) => {
                let mut mtbl = min(outer_mtbl, inner_mtbl);
                let mut expr = self.expr.deref(a);
                if let Mutable = inner_mtbl {
                    // Reborrow
                    if ctx.options.downgrade_shared_inside_shared {
                        mtbl = min(mtbl, self.expr.scrutinee_access_level());
                    }
                    // TODO downgrade if `downgrade_shared_inside_shared`
                    expr = expr.deref(a).borrow(a, mtbl)
                }
                Ok((
                    Rule::ConstructorMultiRef,
                    vec![Self {
                        pat: self.pat,
                        expr,
                    }],
                ))
            }
            (P::Tuple(_), _) => Err(TypeError::TypeMismatch),

            // Dereference rules
            (P::Ref(p_mtbl, p_inner), T::Ref(mut t_mtbl, _)) => {
                let mut expr = self.expr;
                if !ctx.options.eat_inherited_ref_alone
                    && !matches!(type_of_underlying_place, T::Ref(..))
                {
                    // The underlying place is not a reference, so we can't eat the inherited
                    // reference.
                    return Err(TypeError::TypeMismatch);
                }
                if ctx.options.eat_two_layers
                    && let T::Ref(inner_mtbl, _) = type_of_underlying_place
                    && matches!(bm, ByRef(..))
                {
                    expr = expr.reset_binding_mode();
                    t_mtbl = *inner_mtbl;
                }
                match (p_mtbl, t_mtbl) {
                    (Shared, Shared) | (Mutable, Mutable) => Ok((
                        Rule::Deref,
                        vec![Self {
                            pat: p_inner,
                            expr: expr.deref(a),
                        }],
                    )),
                    (Shared, Mutable) if ctx.options.allow_ref_pat_on_ref_mut => Ok((
                        Rule::Deref,
                        vec![Self {
                            pat: self.pat,
                            expr: expr.cast_as_imm_ref(a),
                        }],
                    )),
                    (Shared, Mutable) | (Mutable, Shared) => Err(TypeError::MutabilityMismatch),
                }
            }
            (P::Ref(..), _) => Err(TypeError::TypeMismatch),

            // Binding rules
            (P::Binding(mtbl, ByRef(by_ref_mtbl), name), _) => {
                match (bm, ctx.options.ref_on_ref) {
                    // Easy case: we borrow the expression as expected. We rely on rust's lifetime
                    // extension of temporaries in expressions like `&&x`.
                    (ByMove, _) | (_, RefOnRefBehavior::AllocTemporary) => Ok((
                        Rule::Binding,
                        vec![Self {
                            pat: P::Binding(mtbl, ByMove, name).alloc(a),
                            expr: self.expr.borrow(a, by_ref_mtbl),
                        }],
                    )),
                    // To replicate stable rust behavior, we inspect the binding mode and skip it.
                    // This amounts to getting ahold of the referenced place and re-borrowing it
                    // with the requested mutability.
                    (ByRef(_), RefOnRefBehavior::Skip) => Ok((
                        Rule::Binding,
                        vec![Self {
                            pat: P::Binding(mtbl, ByMove, name).alloc(a),
                            expr: self.expr.reset_binding_mode().borrow(a, by_ref_mtbl),
                        }],
                    )),
                    (ByRef(_), RefOnRefBehavior::Error) => Err(TypeError::RefOnRef),
                }
            }
            (P::Binding(Mutable, ByMove, _), _) => {
                match (bm, ctx.options.mut_on_ref) {
                    // Easy case: declare the binding as expected.
                    (ByMove, _) | (_, MutOnRefBehavior::Keep) => Ok((Rule::Binding, vec![])),
                    // To replicate stable rust behavior, we reset the binding mode.
                    (ByRef(_), MutOnRefBehavior::ResetBindingMode) => Ok((
                        Rule::Binding,
                        vec![Self {
                            pat: self.pat,
                            expr: self.expr.reset_binding_mode(),
                        }],
                    )),
                    (ByRef(_), MutOnRefBehavior::Error) => Err(TypeError::RefOnRef),
                }
            }
            (P::Binding(Shared, ByMove, _), _) => Ok((Rule::Binding, vec![])),
        }
    }

    /// Whether this predicate is completed, i.e. is a simple binding pattern.
    pub fn is_done(&self) -> bool {
        matches!(self.pat, Pattern::Binding(_, BindingMode::ByMove, _))
    }
}
