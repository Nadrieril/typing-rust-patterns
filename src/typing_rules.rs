use serde::{Deserialize, Serialize};
use std::cmp::min;

use crate::*;
use BindingMode::*;
use Mutability::*;

/// What to do to a `ref x` binding to an `&p` or `&mut p` expression (as opposed to an inner place
/// of the scrutinee).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RefBindingOnInheritedBehavior {
    /// Borrow that expression, which requires allocating a temporary variable.
    AllocTemporary,
    /// Stable rust behavior: skip the borrow in the expression and re-borrow the inner.
    Skip,
    /// Treat this as an error.
    Error,
}

/// What to do to a `mut x` binding to an `&p` or `&mut p` expression (as opposed to an inner place
/// of the scrutinee).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum MutBindingOnInheritedBehavior {
    /// Stable rust behavior: reset the binding mode.
    ResetBindingMode,
    /// Declare the expected binding and make it mutable.
    Keep,
    /// Treat this as an error. This is RFC3627 rule 1.
    Error,
}

/// What to do when a reference pattern encounters a double-reference type where the outer one is
/// inherited.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum InheritedRefOnRefBehavior {
    /// Eat only the outer one.
    EatOuter,
    /// Eat the inner one, keeping the outer one (aka binding mode). This is RFC3627 rule 2.
    EatInner,
    /// Stable rust behavior: the ref pattern consumes both layers of reference type.
    EatBoth,
}

/// Choice of typing rules.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RuleOptions {
    pub ref_binding_on_inherited: RefBindingOnInheritedBehavior,
    pub mut_binding_on_inherited: MutBindingOnInheritedBehavior,
    pub inherited_ref_on_ref: InheritedRefOnRefBehavior,
    /// Whether a `&p` pattern is allowed on `&mut T`. This is RFC3627 rule 5.
    pub allow_ref_pat_on_ref_mut: bool,
    /// Whether to simplify some expressions, which removes some borrow errors involving mixes of
    /// `&mut` and `&`.
    pub simplify_expressions: bool,
    /// If false, a reference pattern is only allowed if the _underlying place_ has a compatible
    /// reference type. This is RFC3627 rule 4.
    pub eat_inherited_ref_alone: bool,
    /// If we've dereferenced a shared reference, any subsequent `&mut` inherited reference becomes
    /// `&`. This is RFC3627 rule 3.
    pub downgrade_shared_inside_shared: bool,
}

impl RuleOptions {
    /// Reproduces stable rust behavior.
    pub const STABLE_RUST: Self = RuleOptions {
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::Skip,
        mut_binding_on_inherited: MutBindingOnInheritedBehavior::ResetBindingMode,
        inherited_ref_on_ref: InheritedRefOnRefBehavior::EatBoth,
        allow_ref_pat_on_ref_mut: false,
        simplify_expressions: false,
        eat_inherited_ref_alone: false,
        downgrade_shared_inside_shared: false,
    };

    /// Reproduces RFC3627 (match ergonomics 2024) behavior
    pub const ERGO2024: Self = RuleOptions {
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::Skip,
        mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
        inherited_ref_on_ref: InheritedRefOnRefBehavior::EatInner,
        allow_ref_pat_on_ref_mut: true,
        simplify_expressions: true,
        eat_inherited_ref_alone: true,
        downgrade_shared_inside_shared: true,
    };

    /// A fairly permissive proposal.
    pub const PERMISSIVE: Self = RuleOptions {
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::AllocTemporary,
        mut_binding_on_inherited: MutBindingOnInheritedBehavior::Keep,
        inherited_ref_on_ref: InheritedRefOnRefBehavior::EatOuter,
        allow_ref_pat_on_ref_mut: true,
        simplify_expressions: true,
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
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
        ..Self::PERMISSIVE
    };

    /// The known bundles, with a short explanation.
    pub const KNOWN_OPTION_BUNDLES: &[(&str, Self, &str)] = &[
        ("default", Self::NADRIS_PROPOSAL, "the default settings"),
        (
            "permissive",
            Self::PERMISSIVE,
            "an even more permissive proposal than the default",
        ),
        (
            "stateless",
            Self::STATELESS,
            "a proposal that tracks no hidden state; purely type-based",
        ),
        ("ergo2024", Self::ERGO2024, "wip emulation of RFC3627 rules"),
        (
            "stable_rust",
            Self::STABLE_RUST,
            "emulates the behavior of current stable rust",
        ),
    ];

    pub fn get_bundle_name(self) -> Option<&'static str> {
        Self::KNOWN_OPTION_BUNDLES
            .iter()
            .find(|(_, bundle, _)| *bundle == self)
            .map(|(name, _, _)| *name)
    }
    pub fn from_bundle_name(name: &str) -> Option<Self> {
        Self::KNOWN_OPTION_BUNDLES
            .iter()
            .find(|(n, _, _)| *n == name)
            .map(|(_, bundle, _)| *bundle)
    }

    pub fn to_map(&self) -> serde_json::Map<String, serde_json::Value> {
        let serde_json::Value::Object(map) = serde_json::to_value(self).unwrap() else {
            panic!()
        };
        map
    }
    pub fn set_key(&mut self, key: &str, val: &str) -> anyhow::Result<()> {
        fn from_str<T: for<'de> Deserialize<'de>>(s: &str) -> anyhow::Result<T> {
            let v = serde_yaml::from_str(&s)?;
            Ok(v)
        }
        match key {
            "ref_binding_on_inherited" => self.ref_binding_on_inherited = from_str(val)?,
            "mut_binding_on_inherited" => self.mut_binding_on_inherited = from_str(val)?,
            "inherited_ref_on_ref" => self.inherited_ref_on_ref = from_str(val)?,
            "allow_ref_pat_on_ref_mut" => self.allow_ref_pat_on_ref_mut = from_str(val)?,
            "simplify_expressions" => self.simplify_expressions = from_str(val)?,
            "eat_inherited_ref_alone" => self.eat_inherited_ref_alone = from_str(val)?,
            _ => anyhow::bail!("unknown key `{key}`"),
        }
        Ok(())
    }
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
                            expr: e.borrow(a, Shared, false),
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
                        let expr = self.expr.deref(a).field(a, i).borrow(
                            a,
                            mtbl,
                            ctx.options.downgrade_shared_inside_shared,
                        );
                        Self { pat, expr }
                    })
                    .collect();
                Ok((Rule::ConstructorRef, preds))
            }
            (P::Tuple(_), T::Ref(outer_mtbl, &T::Ref(inner_mtbl, _))) => {
                let mtbl = min(outer_mtbl, inner_mtbl);
                let mut expr = self.expr.deref(a);
                if let Mutable = inner_mtbl {
                    // Reborrow
                    expr = expr
                        .deref(a)
                        .borrow(a, mtbl, ctx.options.downgrade_shared_inside_shared)
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
                if ctx.options.inherited_ref_on_ref == InheritedRefOnRefBehavior::EatBoth
                    && let T::Ref(inner_mtbl, _) = type_of_underlying_place
                    && matches!(bm, ByRef(..))
                {
                    expr = expr.reset_binding_mode();
                    t_mtbl = *inner_mtbl;
                }
                let mut reborrow_after = None;
                if ctx.options.inherited_ref_on_ref == InheritedRefOnRefBehavior::EatInner
                    && let T::Ref(inner_mtbl, _) = type_of_underlying_place
                    && matches!(bm, ByRef(..))
                {
                    expr = expr.reset_binding_mode();
                    reborrow_after = Some(t_mtbl);
                    t_mtbl = *inner_mtbl;
                }
                let mut pred = match (p_mtbl, t_mtbl) {
                    (Shared, Shared) | (Mutable, Mutable) => Self {
                        pat: p_inner,
                        expr: expr.deref(a),
                    },
                    (Shared, Mutable) if ctx.options.allow_ref_pat_on_ref_mut => Self {
                        pat: self.pat,
                        expr: expr.cast_as_imm_ref(a),
                    },
                    (Shared, Mutable) | (Mutable, Shared) => {
                        return Err(TypeError::MutabilityMismatch)
                    }
                };
                if let Some(mtbl) = reborrow_after {
                    pred = Self {
                        pat: pred.pat,
                        expr: pred
                            .expr
                            .borrow(a, mtbl, ctx.options.downgrade_shared_inside_shared),
                    }
                }
                Ok((Rule::Deref, vec![pred]))
            }
            (P::Ref(..), _) => Err(TypeError::TypeMismatch),

            // Binding rules
            (P::Binding(mtbl, ByRef(by_ref_mtbl), name), _) => {
                match (bm, ctx.options.ref_binding_on_inherited) {
                    // Easy case: we borrow the expression as expected. We rely on rust's lifetime
                    // extension of temporaries in expressions like `&&x`.
                    (ByMove, _) | (_, RefBindingOnInheritedBehavior::AllocTemporary) => Ok((
                        Rule::Binding,
                        vec![Self {
                            pat: P::Binding(mtbl, ByMove, name).alloc(a),
                            expr: self.expr.borrow(a, by_ref_mtbl, false),
                        }],
                    )),
                    // To replicate stable rust behavior, we inspect the binding mode and skip it.
                    // This amounts to getting ahold of the referenced place and re-borrowing it
                    // with the requested mutability.
                    (ByRef(_), RefBindingOnInheritedBehavior::Skip) => Ok((
                        Rule::Binding,
                        vec![Self {
                            pat: P::Binding(mtbl, ByMove, name).alloc(a),
                            expr: self.expr.reset_binding_mode().borrow(a, by_ref_mtbl, false),
                        }],
                    )),
                    (ByRef(_), RefBindingOnInheritedBehavior::Error) => Err(TypeError::RefOnRef),
                }
            }
            (P::Binding(Mutable, ByMove, _), _) => {
                match (bm, ctx.options.mut_binding_on_inherited) {
                    // Easy case: declare the binding as expected.
                    (ByMove, _) | (_, MutBindingOnInheritedBehavior::Keep) => {
                        Ok((Rule::Binding, vec![]))
                    }
                    // To replicate stable rust behavior, we reset the binding mode.
                    (ByRef(_), MutBindingOnInheritedBehavior::ResetBindingMode) => Ok((
                        Rule::Binding,
                        vec![Self {
                            pat: self.pat,
                            expr: self.expr.reset_binding_mode(),
                        }],
                    )),
                    (ByRef(_), MutBindingOnInheritedBehavior::Error) => Err(TypeError::MutOnRef),
                }
            }
            (P::Binding(Shared, ByMove, _), _) => Ok((Rule::Binding, vec![])),
        }
    }

    /// Whether this predicate is completed, i.e. is a simple binding pattern.
    pub fn is_done(&self) -> bool {
        matches!(self.pat, Pattern::Binding(_, ByMove, _))
    }
}
