use bincode::{Decode, Encode};
use itertools::Itertools;
use serde::{Deserialize, Serialize};

use crate::*;

/// What to do to a `ref x` binding to an `&p` or `&mut p` expression (as opposed to an inner place
/// of the scrutinee).
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Encode, Decode)]
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Encode, Decode)]
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
#[derive(
    Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize, Encode, Decode,
)]
pub enum InheritedRefOnRefBehavior {
    /// Eat only the outer one.
    EatOuter,
    /// Stable rust behavior: the ref pattern consumes both layers of reference type.
    EatBoth,
    /// Eat the inner one if possible, keeping the outer one (aka binding mode). This is RFC3627 rule 2.
    EatInner,
}

/// Choice of typing rules.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize, Encode, Decode)]
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
    /// In `EatInner` or `EatBoth`, allow eating an inner `&mut T` with `&mut p` from under a `&`.
    pub eat_mut_inside_shared: bool,
    /// What happens with a `ref mut? x` binding and an inherited reference.
    pub ref_binding_on_inherited: RefBindingOnInheritedBehavior,
    /// What happens with a `mut x` binding and an inherited reference.
    pub mut_binding_on_inherited: MutBindingOnInheritedBehavior,
    pub always_inspect_bm: bool,
}

impl RuleOptions {
    /// List options that can be changed without affecting the current rules.
    pub fn irrelevant_options(self) -> &'static [&'static str] {
        if !self.match_constructor_through_ref {
            &[
                "eat_inherited_ref_alone",
                "inherited_ref_on_ref",
                "fallback_to_outer",
                "downgrade_mut_inside_shared",
                "eat_mut_inside_shared",
                "ref_binding_on_inherited",
                "mut_binding_on_inherited",
            ]
        } else if matches!(
            self.inherited_ref_on_ref,
            InheritedRefOnRefBehavior::EatOuter
        ) {
            &["fallback_to_outer", "eat_mut_inside_shared"]
        } else {
            &[]
        }
    }

    pub fn to_map(&self) -> serde_json::Map<String, serde_json::Value> {
        let serde_json::Value::Object(map) = serde_json::to_value(self).unwrap() else {
            panic!()
        };
        map
    }

    pub fn get_key(&self, key: &str) -> String {
        serde_yaml::to_string(&self.to_map()[key])
            .unwrap()
            .trim()
            .to_string()
    }

    pub fn set_key(&mut self, key: &str, val: &str) {
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
        *self = serde_yaml::from_str(&text).unwrap();
    }
}

/// Documentation for the options.
pub const TY_BASED_OPTIONS_DOC: &[OptionsDoc] = &[
    OptionsDoc {
        name: "match_constructor_through_ref",
        doc: "Whether `[p]` can match on `&[T]`; the heart of match ergonomics.",
        values: &[
            OptionValue {
                name: "false",
                doc: "Disable match ergonomics entirely",
            },
            OptionValue {
                name: "true",
                doc: "Allow `[p]` to match on `&[T]`; the heart of match ergonomics",
            },
        ],
    },
    OptionsDoc {
        name: "eat_inherited_ref_alone",
        doc: "Whether `&p`/`&mut p` is allowed on an inherited reference \
               if the underlying type isn't also a reference type",
        values: &[
            OptionValue {
                name: "false",
                doc: "Disallow `&p`/`&mut p` on an inherited reference \
                        if the underlying type isn't also a reference type",
            },
            OptionValue {
                name: "true",
                doc: "Allow `&p`/`&mut p` on an inherited reference \
                        if the underlying type isn't also a reference type",
            },
        ],
    },
    OptionsDoc {
        name: "inherited_ref_on_ref",
        doc: "How to handle a reference pattern on a \
                double reference when the outer one is inherited",
        values: &[
            OptionValue {
                name: "EatOuter",
                doc: "When matching a reference pattern on a \
                        double reference with the outer one being inherited, \
                        match against the outer reference.",
            },
            OptionValue {
                name: "EatInner",
                doc: "When matching a reference pattern on a \
                        double reference with the outer one being inherited, \
                        match against the inner reference.",
            },
            OptionValue {
                name: "EatBoth",
                doc: "When matching a reference pattern on a \
                        double reference with the outer one being inherited, \
                        match against the inner reference and consume both references.",
            },
        ],
    },
    OptionsDoc {
        name: "fallback_to_outer",
        doc: "Whether to try again in `EatOuter` mode when a `EatBoth` or `EatInner` \
                case has a mutability mismatch",
        values: &[
            OptionValue {
                name: "false",
                doc: "Don't try matching on the outer reference if \
                        matching on the inner reference caused a mutability mismatch",
            },
            OptionValue {
                name: "true",
                doc: "Try matching on the outer reference if \
                        matching on the inner reference caused a mutability mismatch",
            },
        ],
    },
    OptionsDoc {
        name: "eat_mut_inside_shared",
        doc: "In `EatInner` or `EatBoth`, `&mut p` can eat an inner `&mut T` \
                from under a `&`",
        values: &[
            OptionValue {
                name: "false",
                doc: "Disallow matching a `&mut p` pattern against an inner reference, \
                         if the outer reference type is shared",
            },
            OptionValue {
                name: "true",
                doc: "Allow matching a `&mut p` pattern against an inner reference, \
                         even if the outer reference type is shared",
            },
        ],
    },
    OptionsDoc {
        name: "allow_ref_pat_on_ref_mut",
        doc: "Whether to allow a shared ref pattern on a mutable ref type",
        values: &[
            OptionValue {
                name: "false",
                doc: "Disallow a shared ref pattern to match on a `&mut T` as if it was `&T`",
            },
            OptionValue {
                name: "true",
                doc: "Allow a shared ref pattern to match on a `&mut T` as if it was `&T`",
            },
        ],
    },
    OptionsDoc {
        name: "downgrade_mut_inside_shared",
        doc: "RFC3627 rule 3: downgrade `&mut` inherited references \
                to `&` inside a shared deref",
        values: &[
            OptionValue {
                name: "false",
                doc: "RFC3627 rule 3: don't downgrade `&mut` inherited references \
                        to `&` inside a shared deref",
            },
            OptionValue {
                name: "true",
                doc: "RFC3627 rule 3: downgrade `&mut` inherited references \
                        to `&` inside a shared deref",
            },
        ],
    },
    OptionsDoc {
        name: "ref_binding_on_inherited",
        doc: "How to handle a `ref x` binding on an inherited reference",
        values: &[
            OptionValue {
                name: "Error",
                doc: "Disallow a `ref x` binding on an inherited reference",
            },
            OptionValue {
                name: "ResetBindingMode",
                doc: "When a `ref x` binding matches on an inherited reference, \
                        remove the inherited reference before assigning the binding",
            },
            OptionValue {
                name: "AllocTemporary",
                doc: "Allow a `ref x` binding on an inherited reference, \
                        thereby creating a temporary place to borrow from",
            },
        ],
    },
    OptionsDoc {
        name: "mut_binding_on_inherited",
        doc: "How to handle a `mut x` binding on an inherited reference",
        values: &[
            OptionValue {
                name: "Error",
                doc: "Disallow a `mut x` binding on an inherited reference",
            },
            OptionValue {
                name: "ResetBindingMode",
                doc: "When a `mut x` binding matches on an inherited reference, \
                        remove the inherited reference before assigning the binding",
            },
            OptionValue {
                name: "Keep",
                doc: "Allow a `mut x` binding on an inherited reference",
            },
        ],
    },
    OptionsDoc {
        name: "simplify_deref_mut",
        doc: "Whether to simplify `*&mut expr`, which removes some borrow errors",
        values: &[
            OptionValue {
                name: "false",
                doc: "TODO",
            },
            OptionValue {
                name: "true",
                doc: "TODO",
            },
        ],
    },
    OptionsDoc {
        name: "always_inspect_bm",
        doc: "Whether to always branch on the binding mode when computing rules. \
                this is required for the `SequentBindingMode` style",
        values: &[
            OptionValue {
                name: "false",
                doc: "TODO",
            },
            OptionValue {
                name: "true",
                doc: "TODO",
            },
        ],
    },
];

impl RuleOptions {
    /// Reproduces stable rust behavior.
    pub const STABLE_RUST: Self = RuleOptions {
        match_constructor_through_ref: true,
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::ResetBindingMode,
        mut_binding_on_inherited: MutBindingOnInheritedBehavior::ResetBindingMode,
        inherited_ref_on_ref: InheritedRefOnRefBehavior::EatBoth,
        fallback_to_outer: false,
        allow_ref_pat_on_ref_mut: false,
        simplify_deref_mut: true,
        eat_inherited_ref_alone: false,
        downgrade_mut_inside_shared: false,
        eat_mut_inside_shared: true,
        always_inspect_bm: false,
    };

    /// Reproduces RFC3627 (match ergonomics 2024) behavior
    pub const ERGO2024: Self = RuleOptions {
        match_constructor_through_ref: true,
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::ResetBindingMode,
        mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
        inherited_ref_on_ref: InheritedRefOnRefBehavior::EatInner,
        fallback_to_outer: true,
        allow_ref_pat_on_ref_mut: true,
        simplify_deref_mut: true,
        eat_inherited_ref_alone: true,
        downgrade_mut_inside_shared: true,
        eat_mut_inside_shared: true,
        always_inspect_bm: false,
    };

    /// A fairly permissive proposal, with the benefit of requiring 0 implicit state: we never
    /// inspect the DBM, we only follow the types.
    pub const STATELESS: Self = RuleOptions {
        match_constructor_through_ref: true,
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::AllocTemporary,
        mut_binding_on_inherited: MutBindingOnInheritedBehavior::Keep,
        inherited_ref_on_ref: InheritedRefOnRefBehavior::EatOuter,
        fallback_to_outer: false,
        allow_ref_pat_on_ref_mut: true,
        simplify_deref_mut: true,
        eat_inherited_ref_alone: true,
        downgrade_mut_inside_shared: false,
        eat_mut_inside_shared: true,
        always_inspect_bm: false,
    };

    /// The default setting for the solver. A reasonable proposal.
    pub const NADRI: Self = RuleOptions {
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
        ..Self::STATELESS
    };

    /// Purely structural matching, with no match ergonomics.
    pub const STRUCTURAL: Self = RuleOptions {
        match_constructor_through_ref: false,
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
        mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
        fallback_to_outer: false,
        allow_ref_pat_on_ref_mut: false,
        eat_inherited_ref_alone: false,
        eat_mut_inside_shared: false,
        ..Self::STATELESS
    };

    pub const RFC3627_2021: Self = RuleOptions {
        mut_binding_on_inherited: MutBindingOnInheritedBehavior::ResetBindingMode,
        inherited_ref_on_ref: InheritedRefOnRefBehavior::EatBoth,
        fallback_to_outer: true,
        ..RuleOptions::ERGO2024
    };

    pub const ERGO2024_BREAKING_ONLY: Self = RuleOptions {
        mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
        inherited_ref_on_ref: InheritedRefOnRefBehavior::EatInner,
        fallback_to_outer: false,
        ..RuleOptions::STABLE_RUST
    };

    pub const ERGO2024_BREAKING_ONLY_EXT: Self = RuleOptions {
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
        eat_mut_inside_shared: false,
        ..RuleOptions::ERGO2024_BREAKING_ONLY
    };

    pub const WAFFLE: Self = RuleOptions {
        inherited_ref_on_ref: InheritedRefOnRefBehavior::EatOuter,
        allow_ref_pat_on_ref_mut: false,
        ..Self::ERGO2024
    };

    pub fn get_bundle_name(self) -> Option<&'static str> {
        RuleSet::TypeBased(self).get_bundle_name()
    }

    pub fn from_bundle_name(name: &str) -> Option<Self> {
        KNOWN_TY_BASED_BUNDLES
            .iter()
            .find(|b| b.name == name)
            .map(|b| b.ruleset)
    }
}

/// The known bundles, with a short explanation.
pub static KNOWN_TY_BASED_BUNDLES: &[BundleDoc<RuleOptions>] = &[
    BundleDoc {
        name: "nadri",
        ruleset: RuleOptions::NADRI,
        doc: "A reasonable proposal; like `stateless` but \
                forbids `ref` bindings that create temporaries",
    },
    BundleDoc {
        name: "stateless",
        ruleset: RuleOptions::STATELESS,
        doc: "A proposal that tracks no hidden state; purely type-based",
    },
    BundleDoc {
        name: "stable_rust",
        ruleset: RuleOptions::STABLE_RUST,
        doc: "The behavior of current stable rust",
    },
    BundleDoc {
        name: "rfc3627",
        ruleset: RuleOptions::ERGO2024,
        doc: "The accepted RFC3627 behavior",
    },
    BundleDoc {
        name: "rfc3627_2021",
        ruleset: RuleOptions::RFC3627_2021,
        doc: "The accepted RFC3627 behavior under edition 2021",
    },
    BundleDoc {
        name: "rfc3627_2024_min",
        ruleset: RuleOptions::ERGO2024_BREAKING_ONLY,
        doc: "The breaking changes for edition 2024 planned in RFC3627",
    },
    BundleDoc {
        name: "structural",
        ruleset: RuleOptions::STRUCTURAL,
        doc: "Purely structural matching, with no match ergonomics",
    },
    BundleDoc {
        name: "waffle",
        ruleset: RuleOptions::WAFFLE,
        doc: "A proposal by @WaffleLapkin (excluding the proposed rule3 extension)",
    },
    // ("rpjohnst", RuleOptions::RPJOHNST, "(TODO) a proposal by @rpjohnst"),
];
