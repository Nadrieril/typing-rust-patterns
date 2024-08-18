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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
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
    pub simplify_deref_mut: bool,
    /// If false, a reference pattern is only allowed if the _underlying place_ has a compatible
    /// reference type. This is RFC3627 rule 4.
    pub eat_inherited_ref_alone: bool,
    /// If we've dereferenced a shared reference, any subsequent `&mut` inherited reference becomes
    /// `&`. This is RFC3627 rule 3.
    pub downgrade_shared_inside_shared: bool,
    /// How to display rules.
    pub rules_display_style: TypingRuleStyle,
}

impl RuleOptions {
    /// Documentation for the options.
    pub const OPTIONS_DOC: &[(&str, &str, &str)] = &[
        (
            "rules_display_style",
            "Plain | BindingMode",
            "how to display typing rules (in the `rules` command)",
        ),
        (
            "ref_binding_on_inherited",
            "AllocTemporary | Skip | Error",
            "how to handle a `ref x` binding on an inherited reference",
        ),
        (
            "mut_binding_on_inherited",
            "ResetBindingMode | Keep | Error",
            "how to handle a `mut x` binding on an inherited reference",
        ),
        (
            "inherited_ref_on_ref",
            "EatOuter | EatInner | EatBoth",
            "how to handle a reference pattern on a \
             double reference when the outer one is inherited",
        ),
        (
            "allow_ref_pat_on_ref_mut",
            "bool",
            "whether to allow `&p: &mut T`",
        ),
        (
            "simplify_deref_mut",
            "bool",
            "whether to simplify `*&mut expr`, which removes some borrow errors",
        ),
        (
            "eat_inherited_ref_alone",
            "bool",
            "whether `&p: &T` is allowed if the reference is inherited and `T` isn't some `&U`",
        ),
        (
            "downgrade_shared_inside_shared",
            "bool",
            "RFC3627 rule 3: downgrade `&mut` inherited references to `&` inside a shared deref",
        ),
    ];

    pub fn set_key(&mut self, key: &str, val: &str) -> anyhow::Result<()> {
        fn from_str<T: for<'de> Deserialize<'de>>(s: &str) -> anyhow::Result<T> {
            let v = serde_yaml::from_str(&s)?;
            Ok(v)
        }
        match key {
            "rules_display_style" => self.rules_display_style = from_str(val)?,
            "ref_binding_on_inherited" => self.ref_binding_on_inherited = from_str(val)?,
            "mut_binding_on_inherited" => self.mut_binding_on_inherited = from_str(val)?,
            "inherited_ref_on_ref" => self.inherited_ref_on_ref = from_str(val)?,
            "allow_ref_pat_on_ref_mut" => self.allow_ref_pat_on_ref_mut = from_str(val)?,
            "simplify_deref_mut" => self.simplify_deref_mut = from_str(val)?,
            "eat_inherited_ref_alone" => self.eat_inherited_ref_alone = from_str(val)?,
            "downgrade_shared_inside_shared" => {
                self.downgrade_shared_inside_shared = from_str(val)?
            }
            _ => anyhow::bail!("unknown key `{key}`"),
        }
        Ok(())
    }

    pub fn to_map(&self) -> serde_json::Map<String, serde_json::Value> {
        let serde_json::Value::Object(map) = serde_json::to_value(self).unwrap() else {
            panic!()
        };
        map
    }
}

/// The various typing rules we can apply.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Rule {
    Constructor,
    ConstructorRef,
    ConstructorMultiRef,
    Deref(InheritedRefOnRefBehavior),
    DerefMutWithShared(InheritedRefOnRefBehavior),
    BindingOverrideBorrow,
    BindingResetBindingMode,
    BindingBorrow,
    Binding,
    ExprSimplification,
}

#[derive(Debug, Clone, Copy)]
pub enum TypeError {
    TypeMismatch,
    MutabilityMismatch,
    RefOnRef,
    MutOnRef,
    OverlyGeneralPattern,
    OverlyGeneralType,
    OverlyGeneralExpr,
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

    /// Apply one step of rule to this predicate.
    /// Note: The `downgrade_shared_inside_shared` option is special: it inspects `self.expr` in
    /// non-trivial ways.
    /// All the other rules inspect `self.pat`, `self.expr.ty`, `self.expr.binding_mode()` up to a
    /// fixed depth. We trigger `OverlyGeneral` errors it the rules needs more information to
    /// decide what to do. This is how we can auto-generate the rules listings.
    pub fn step(&self, ctx: TypingCtx<'a>) -> Result<(Rule, Vec<Self>), TypeError> {
        use Pattern as P;
        use Type as T;
        let a = ctx.arenas;

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
            (P::Tuple(_), T::Tuple(_)) => Err(TypeError::TypeMismatch),
            (P::Tuple(pats), T::Ref(mtbl, T::Tuple(tys))) if pats.len() == tys.len() => {
                let preds = pats
                    .iter()
                    .enumerate()
                    .map(|(i, pat)| {
                        let expr = self.expr.deref(a).field(a, i).borrow_cap_mutability(
                            a,
                            mtbl,
                            ctx.options.downgrade_shared_inside_shared,
                        );
                        Self { pat, expr }
                    })
                    .collect();
                Ok((Rule::ConstructorRef, preds))
            }
            (P::Tuple(_), T::Ref(_, T::Tuple(_))) => Err(TypeError::TypeMismatch),
            (P::Tuple(_), T::Ref(outer_mtbl, &T::Ref(inner_mtbl, _))) => {
                let mtbl = min(outer_mtbl, inner_mtbl);
                // Dereference the expression.
                let mut expr = if ctx.options.simplify_deref_mut {
                    self.expr.deref_or_reset(a)
                } else {
                    self.expr.deref(a)
                };

                if let Mutable = inner_mtbl {
                    // Reborrow
                    expr = expr.deref(a).borrow_cap_mutability(
                        a,
                        mtbl,
                        ctx.options.downgrade_shared_inside_shared,
                    )
                }
                Ok((
                    Rule::ConstructorMultiRef,
                    vec![Self {
                        pat: self.pat,
                        expr,
                    }],
                ))
            }
            (P::Tuple(_), T::Ref(_, T::Var(_))) => Err(TypeError::OverlyGeneralType),
            (P::Tuple(_), T::Var(_)) => Err(TypeError::OverlyGeneralType),

            // Dereference rules
            (P::Ref(p_mtbl, p_inner), T::Ref(mut t_mtbl, _)) => {
                let mut expr = self.expr;
                let mut rule_variant = InheritedRefOnRefBehavior::EatOuter;
                let mut reborrow_after = None;

                // Only inspect the binding mode if there are options that need it.
                if !matches!(
                    ctx.options.inherited_ref_on_ref,
                    InheritedRefOnRefBehavior::EatOuter
                ) || !ctx.options.eat_inherited_ref_alone
                {
                    let bm = self.expr.binding_mode()?;
                    if matches!(bm, ByRef(..)) {
                        // The reference is inherited: options differ in their treatment of this
                        // case.
                        let type_of_underlying_place = self.expr.reset_binding_mode()?.ty;
                        match type_of_underlying_place {
                            T::Ref(inner_mtbl, _) => {
                                rule_variant = ctx.options.inherited_ref_on_ref;
                                match ctx.options.inherited_ref_on_ref {
                                    InheritedRefOnRefBehavior::EatOuter => {}
                                    InheritedRefOnRefBehavior::EatInner => {
                                        expr = expr.reset_binding_mode()?;
                                        reborrow_after = Some(t_mtbl);
                                        t_mtbl = *inner_mtbl;
                                    }
                                    InheritedRefOnRefBehavior::EatBoth => {
                                        expr = expr.reset_binding_mode()?;
                                        t_mtbl = *inner_mtbl;
                                    }
                                }
                            }
                            T::Var(_) => {
                                return Err(TypeError::OverlyGeneralType);
                            }
                            // The underlying place is not a reference, so we can't eat the inherited
                            // reference.
                            T::Tuple(_) if !ctx.options.eat_inherited_ref_alone => {
                                return Err(TypeError::TypeMismatch);
                            }
                            // Continue
                            T::Tuple(_) => {}
                        }
                    }
                }

                // Dereference the expression.
                let expr = if ctx.options.simplify_deref_mut {
                    expr.deref_or_reset(a)
                } else {
                    expr.deref(a)
                };

                // Match the pattern reference against the appropriate type reference.
                let (rule, mut pred) = match (p_mtbl, t_mtbl) {
                    // Dereference the expression, use the inner pattern.
                    (Shared, Shared) | (Mutable, Mutable) => {
                        (Rule::Deref(rule_variant), Self { pat: p_inner, expr })
                    }
                    // Reborrow the expression, continue with the same pattern.
                    (Shared, Mutable) if ctx.options.allow_ref_pat_on_ref_mut => (
                        Rule::DerefMutWithShared(rule_variant),
                        Self {
                            pat: self.pat,
                            expr: expr.borrow(a, Shared),
                        },
                    ),
                    (Shared, Mutable) => return Err(TypeError::MutabilityMismatch),
                    (Mutable, Shared) => return Err(TypeError::MutabilityMismatch),
                };

                if let Some(mtbl) = reborrow_after {
                    pred.expr = pred.expr.borrow_cap_mutability(
                        a,
                        mtbl,
                        ctx.options.downgrade_shared_inside_shared,
                    );
                }

                Ok((rule, vec![pred]))
            }
            (P::Ref(..), T::Tuple(..)) => Err(TypeError::TypeMismatch),
            (P::Ref(..), T::Var(..)) => Err(TypeError::OverlyGeneralType),

            // Binding rules
            (P::Binding(mtbl, ByRef(by_ref_mtbl), name), _)
                if matches!(
                    ctx.options.ref_binding_on_inherited,
                    RefBindingOnInheritedBehavior::AllocTemporary
                ) =>
            {
                // Easy case: we borrow the expression as expected. We rely on rust's lifetime
                // extension of temporaries in expressions like `&&x`.
                Ok((
                    Rule::BindingBorrow,
                    vec![Self {
                        pat: P::Binding(mtbl, ByMove, name).alloc(a),
                        expr: self.expr.borrow(a, by_ref_mtbl),
                    }],
                ))
            }
            (P::Binding(mtbl, ByRef(by_ref_mtbl), name), _) => {
                match self.expr.binding_mode()? {
                    // Easy case: we borrow the expression as expected.
                    ByMove => Ok((
                        Rule::BindingBorrow,
                        vec![Self {
                            pat: P::Binding(mtbl, ByMove, name).alloc(a),
                            expr: self.expr.borrow(a, by_ref_mtbl),
                        }],
                    )),
                    ByRef(_) => {
                        match ctx.options.ref_binding_on_inherited {
                            // To replicate stable rust behavior, we inspect the binding mode and skip it.
                            // This amounts to getting ahold of the referenced place and re-borrowing it
                            // with the requested mutability.
                            RefBindingOnInheritedBehavior::Skip => Ok((
                                Rule::BindingOverrideBorrow,
                                vec![Self {
                                    pat: P::Binding(mtbl, ByMove, name).alloc(a),
                                    expr: self.expr.reset_binding_mode()?.borrow(a, by_ref_mtbl),
                                }],
                            )),
                            RefBindingOnInheritedBehavior::Error => Err(TypeError::RefOnRef),
                            RefBindingOnInheritedBehavior::AllocTemporary => unreachable!(),
                        }
                    }
                }
            }
            (P::Binding(Mutable, ByMove, _), _)
                if matches!(
                    ctx.options.mut_binding_on_inherited,
                    MutBindingOnInheritedBehavior::Keep
                ) =>
            {
                // Easy case: declare the binding as expected.
                Ok((Rule::Binding, vec![]))
            }
            (P::Binding(Mutable, ByMove, _), _) => {
                match self.expr.binding_mode()? {
                    // Easy case: declare the binding as expected.
                    ByMove => Ok((Rule::Binding, vec![])),
                    ByRef(_) => {
                        match ctx.options.mut_binding_on_inherited {
                            // To replicate stable rust behavior, we reset the binding mode.
                            MutBindingOnInheritedBehavior::ResetBindingMode => Ok((
                                Rule::BindingResetBindingMode,
                                vec![Self {
                                    pat: self.pat,
                                    expr: self.expr.reset_binding_mode()?,
                                }],
                            )),
                            MutBindingOnInheritedBehavior::Error => Err(TypeError::MutOnRef),
                            MutBindingOnInheritedBehavior::Keep => unreachable!(),
                        }
                    }
                }
            }
            (P::Binding(Shared, ByMove, _), _) => Ok((Rule::Binding, vec![])),

            (P::Abstract(..), _) => Err(TypeError::OverlyGeneralPattern),
        }
    }
}
