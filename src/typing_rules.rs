use itertools::Itertools;
use serde::{Deserialize, Serialize};
use std::cmp::min;

use crate::*;
use BindingMode::*;
use Mutability::*;

/// What to do to a `ref x` binding to an `&p` or `&mut p` expression (as opposed to an inner place
/// of the scrutinee).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum RefBindingOnInheritedBehavior {
    /// Stable rust behavior: skip the borrow in the expression and re-borrow the inner.
    ResetBindingMode,
    /// Borrow that expression, which requires allocating a temporary variable.
    AllocTemporary,
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
    /// Stable rust behavior: the ref pattern consumes both layers of reference type.
    EatBoth,
    /// Eat the inner one if possible, keeping the outer one (aka binding mode). This is RFC3627 rule 2.
    EatInner,
}

/// Choice of typing rules.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub struct RuleOptions {
    /// Whether `[p]` can match on `&[T]`. The heart of match ergonomics.
    pub match_constructor_through_ref: bool,
    /// If false, a reference pattern can only consider eating an inherited reference if the
    /// underlying place is of reference type.
    pub eat_inherited_ref_alone: bool,
    /// What happens with a `&mut?p` pattern matching on `&mut?&mut?T` where the outer reference is
    /// inherited.
    pub inherited_ref_on_ref: InheritedRefOnRefBehavior,
    /// In the `EatBoth` and `EatInner` cases, if matching against the underlying place fails this
    /// determines whether we try again in `EatOuter` mode.
    pub fallback_to_outer: bool,
    /// Whether a `&p` pattern is allowed on `&mut T`. This is RFC3627 rule 5.
    pub allow_ref_pat_on_ref_mut: bool,
    /// Whether to simplify some expressions, which removes some borrow errors involving mixes of
    /// `&mut` and `&`.
    pub simplify_deref_mut: bool,
    /// If we've dereferenced a shared reference, any subsequent `&mut` inherited reference becomes
    /// `&`. This is RFC3627 rule 3.
    pub downgrade_mut_inside_shared: bool,
    /// In `EatInner` or `EatBoth`, disallow eating an inner `&mut T` with `&mut p` from under a `&`.
    pub dont_eat_mut_inside_shared: bool,
    /// What happens with a `ref mut? x` binding and an inherited reference.
    pub ref_binding_on_inherited: RefBindingOnInheritedBehavior,
    /// What happens with a `mut x` binding and an inherited reference.
    pub mut_binding_on_inherited: MutBindingOnInheritedBehavior,
    // TODO: double_ref: Last | Min
}

impl RuleOptions {
    /// Documentation for the options.
    pub const OPTIONS_DOC: &[(&str, &[&str], &str)] = &[
        (
            "match_constructor_through_ref",
            &["true", "false"],
            "whether `[p]` can match on `&[T]`; the heart of match ergonomics.",
        ),
        (
            "eat_inherited_ref_alone",
            &["true", "false"],
            "whether `&p`/`&mut p` is allowed on an inherited reference if the underlying type isn't also a reference type",
        ),
        (
            "inherited_ref_on_ref",
            &["EatOuter", "EatInner", "EatBoth"],
            "how to handle a reference pattern on a \
             double reference when the outer one is inherited",
        ),
        (
            "fallback_to_outer",
            &["true", "false"],
            "whether to try again in `EatOuter` mode when a `EatBoth` or `EatInner` case has a mutability mismatch",
        ),
        (
            "allow_ref_pat_on_ref_mut",
            &["true", "false"],
            "whether to allow `&p: &mut T`",
        ),
        (
            "simplify_deref_mut",
            &["true", "false"],
            "whether to simplify `*&mut expr`, which removes some borrow errors",
        ),
        (
            "downgrade_mut_inside_shared",
            &["true", "false"],
            "RFC3627 rule 3: downgrade `&mut` inherited references to `&` inside a shared deref",
        ),
        (
            "dont_eat_mut_inside_shared",
            &["true", "false"],
            "in `EatInner` or `EatBoth`, disallow eating an inner `&mut T` with `&mut p` from under a `&`",
        ),
        (
            "ref_binding_on_inherited",
            &["ResetBindingMode", "AllocTemporary", "Error"],
            "how to handle a `ref x` binding on an inherited reference",
        ),
        (
            "mut_binding_on_inherited",
            &["ResetBindingMode", "Keep", "Error"],
            "how to handle a `mut x` binding on an inherited reference",
        ),
    ];

    pub fn set_key(&mut self, key: &str, val: &str) -> anyhow::Result<()> {
        // Hack to set a key without knowing its type: print as a yaml object, replacing the key we
        // care about with the string value we got. Yaml parsing will parse the value correctly.
        let text = self
            .to_map()
            .into_iter()
            .map(|(k, v)| {
                let v = if k == key {
                    val
                } else {
                    &serde_yaml::to_string(&v).unwrap()
                };
                format!("{k}: {v}")
            })
            .join("\n");
        *self = serde_yaml::from_str(&text)?;
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Rule {
    Constructor,
    ConstructorRef(DowngradeMutToRef),
    ConstructorMultiRef(DowngradeMutToRef),
    Deref(
        InheritedRefOnRefBehavior,
        DowngradeMutToRef,
        FallbackToOuter,
    ),
    DerefMutWithShared(InheritedRefOnRefBehavior),
    RefBindingResetBindingMode,
    MutBindingResetBindingMode,
    BindingBorrow,
    Binding,
    ExprSimplification,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum DowngradeMutToRef {
    Normal,
    ForceReadOnly,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct FallbackToOuter(pub bool);

#[derive(Debug, Clone, Copy)]
pub enum TypeError {
    TypeMismatch,
    MutabilityMismatch,
    InheritedRefIsAlone,
    RefOnRef,
    MutOnRef,
    OverlyGeneral(DeepeningRequest),
    /// An error generated by running the `ergonomics-formality` solver.
    External(match_ergonomics_formality::StepError),
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
    /// Note: The `downgrade_mut_inside_shared` option is special: it inspects `self.expr` in
    /// non-trivial ways.
    /// All the other rules inspect `self.pat`, `self.expr.ty`, `self.expr.binding_mode()` up to a
    /// fixed depth. We trigger `OverlyGeneral` errors it the rules needs more information to
    /// decide what to do. This is how we can auto-generate the rules listings.
    pub fn step(&self, ctx: TypingCtx<'a>) -> Result<(Rule, Vec<Self>), TypeError> {
        use DeepeningRequest as D;
        use Pattern as P;
        use Type as T;
        let a = ctx.arenas;
        let o = ctx.options;

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
            (P::Tuple(pats), T::Ref(mut mtbl, T::Tuple(tys)))
                if pats.len() == tys.len() && o.match_constructor_through_ref =>
            {
                let mut downgrade = DowngradeMutToRef::Normal;
                if ctx.options.downgrade_mut_inside_shared && mtbl == Mutable {
                    mtbl = self.expr.scrutinee_mutability()?;
                    if mtbl == Shared {
                        downgrade = DowngradeMutToRef::ForceReadOnly;
                    }
                }
                let preds = pats
                    .iter()
                    .enumerate()
                    .map(|(i, pat)| {
                        let expr = self.expr.deref(a).field(a, i).borrow(a, mtbl);
                        Ok(Self { pat, expr })
                    })
                    .try_collect()?;
                Ok((Rule::ConstructorRef(downgrade), preds))
            }
            (P::Tuple(_), T::Ref(_, T::Tuple(_))) => Err(TypeError::TypeMismatch),
            (P::Tuple(_), T::Ref(outer_mtbl, &T::Ref(inner_mtbl, _)))
                if o.match_constructor_through_ref =>
            {
                let mut mtbl = min(outer_mtbl, inner_mtbl);
                let mut downgrade = DowngradeMutToRef::Normal;
                if ctx.options.downgrade_mut_inside_shared && mtbl == Mutable {
                    mtbl = self.expr.scrutinee_mutability()?;
                    if mtbl == Shared {
                        downgrade = DowngradeMutToRef::ForceReadOnly;
                    }
                }
                let expr = self.expr.deref(a).deref(a).borrow(a, mtbl);
                Ok((
                    Rule::ConstructorMultiRef(downgrade),
                    vec![Self {
                        pat: self.pat,
                        expr,
                    }],
                ))
            }
            (P::Tuple(_), T::Ref(_, T::Ref(..))) => Err(TypeError::TypeMismatch),
            (P::Tuple(_), T::Ref(_, T::Abstract(_) | T::NonRef(..))) => {
                Err(TypeError::OverlyGeneral(D::Type))
            }
            (P::Tuple(_), T::Abstract(_) | T::NonRef(..)) => Err(TypeError::OverlyGeneral(D::Type)),

            // Dereference rules
            (P::Ref(p_mtbl, p_inner), T::Ref(mut t_mtbl, _)) => {
                let mut rule_variant = InheritedRefOnRefBehavior::EatOuter;
                let mut fallback_to_outer = FallbackToOuter(false);
                let mut reborrow_after = None;

                // We only inspect the binding mode if there are options that need it.
                let must_inspect_bm = matches!(
                    ctx.options.inherited_ref_on_ref,
                    InheritedRefOnRefBehavior::EatInner | InheritedRefOnRefBehavior::EatBoth
                ) || !ctx.options.eat_inherited_ref_alone;
                // Construct the dereferenced expression.
                let expr = if must_inspect_bm && let ByRef(bm_mtbl) = self.expr.binding_mode()? {
                    // The reference is inherited; options differ in their treatment of this case.
                    let underlying_place = self.expr.reset_binding_mode()?;
                    match underlying_place.ty {
                        T::Ref(inner_mtbl, _) => {
                            rule_variant = ctx.options.inherited_ref_on_ref;
                            match ctx.options.inherited_ref_on_ref {
                                InheritedRefOnRefBehavior::EatOuter => {
                                    if ctx.options.simplify_deref_mut && bm_mtbl == Mutable {
                                        underlying_place
                                    } else {
                                        self.expr.deref(a)
                                    }
                                }
                                InheritedRefOnRefBehavior::EatInner => {
                                    let can_eat_inner = match (p_mtbl, *inner_mtbl) {
                                        (Shared, Shared) => true,
                                        (Mutable, Mutable) => {
                                            bm_mtbl == Mutable
                                                || !ctx.options.dont_eat_mut_inside_shared
                                        }
                                        (Shared, Mutable) => ctx.options.allow_ref_pat_on_ref_mut,
                                        (Mutable, Shared) => false,
                                    };
                                    if can_eat_inner {
                                        reborrow_after = Some(t_mtbl);
                                        t_mtbl = *inner_mtbl;
                                        underlying_place.deref(a)
                                    } else if o.fallback_to_outer {
                                        fallback_to_outer = FallbackToOuter(true);
                                        if ctx.options.simplify_deref_mut && bm_mtbl == Mutable {
                                            underlying_place
                                        } else {
                                            self.expr.deref(a)
                                        }
                                    } else {
                                        return Err(TypeError::MutabilityMismatch);
                                    }
                                }
                                InheritedRefOnRefBehavior::EatBoth => {
                                    let can_eat_inner = match (p_mtbl, *inner_mtbl) {
                                        (Shared, Shared) => true,
                                        (Mutable, Mutable) => {
                                            bm_mtbl == Mutable
                                                || !ctx.options.dont_eat_mut_inside_shared
                                        }
                                        (Shared, Mutable) => ctx.options.allow_ref_pat_on_ref_mut,
                                        (Mutable, Shared) => false,
                                    };
                                    if can_eat_inner {
                                        t_mtbl = *inner_mtbl;
                                        underlying_place.deref(a)
                                    } else if o.fallback_to_outer {
                                        fallback_to_outer = FallbackToOuter(true);
                                        if ctx.options.simplify_deref_mut && bm_mtbl == Mutable {
                                            underlying_place
                                        } else {
                                            self.expr.deref(a)
                                        }
                                    } else {
                                        return Err(TypeError::MutabilityMismatch);
                                    }
                                }
                            }
                        }
                        T::Tuple(_) | T::NonRef(..) if ctx.options.eat_inherited_ref_alone => {
                            if ctx.options.simplify_deref_mut && bm_mtbl == Mutable {
                                underlying_place
                            } else {
                                self.expr.deref(a)
                            }
                        }
                        T::Tuple(_) | T::NonRef(..) => {
                            return Err(TypeError::InheritedRefIsAlone);
                        }
                        T::Abstract(_) => {
                            return Err(TypeError::OverlyGeneral(D::Type));
                        }
                    }
                } else {
                    self.expr.deref(a)
                };

                // Match the pattern reference against the appropriate type reference.
                let (mut rule, mut expr) = match (p_mtbl, t_mtbl) {
                    (Shared, Shared) | (Mutable, Mutable) => (
                        Rule::Deref(rule_variant, DowngradeMutToRef::Normal, fallback_to_outer),
                        expr,
                    ),
                    (Shared, Mutable) if ctx.options.allow_ref_pat_on_ref_mut => (
                        Rule::DerefMutWithShared(rule_variant),
                        expr.borrow(a, Shared).deref(a),
                    ),
                    (Shared, Mutable) | (Mutable, Shared) => {
                        return Err(TypeError::MutabilityMismatch)
                    }
                };

                if let Some(mut mtbl) = reborrow_after {
                    if ctx.options.downgrade_mut_inside_shared && mtbl == Mutable {
                        mtbl = expr.scrutinee_mutability()?;
                        if mtbl == Shared
                            && let Rule::Deref(_, downgrade, _) = &mut rule
                        {
                            *downgrade = DowngradeMutToRef::ForceReadOnly;
                        }
                    }
                    // If we were matching under the inherited reference, we restore it here.
                    expr = expr.borrow(a, mtbl);
                }

                let pred = Self { pat: p_inner, expr };
                Ok((rule, vec![pred]))
            }
            (P::Ref(..), T::Tuple(..) | T::NonRef(..)) => Err(TypeError::TypeMismatch),
            (P::Ref(..), T::Abstract(..)) => Err(TypeError::OverlyGeneral(D::Type)),

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
                            RefBindingOnInheritedBehavior::ResetBindingMode => Ok((
                                Rule::RefBindingResetBindingMode,
                                vec![Self {
                                    pat: self.pat,
                                    expr: self.expr.reset_binding_mode()?,
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
                                Rule::MutBindingResetBindingMode,
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

            (P::Abstract(..), _) => Err(TypeError::OverlyGeneral(D::Pattern)),
        }
    }
}
