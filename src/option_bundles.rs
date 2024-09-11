use crate::*;

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
        dont_eat_mut_inside_shared: false,
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
        dont_eat_mut_inside_shared: false,
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
        dont_eat_mut_inside_shared: false,
        always_inspect_bm: false,
    };

    /// The default setting for the solver. A reasonable proposal.
    pub const DEFAULT: Self = RuleOptions {
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
        ..Self::STATELESS
    };

    /// Purely structural matching, with no match ergonomics.
    pub const STRUCTURAL: Self = RuleOptions {
        match_constructor_through_ref: false,
        allow_ref_pat_on_ref_mut: false,
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
        dont_eat_mut_inside_shared: true,
        ..RuleOptions::ERGO2024_BREAKING_ONLY
    };

    pub const WAFFLE: Self = RuleOptions {
        inherited_ref_on_ref: InheritedRefOnRefBehavior::EatOuter,
        allow_ref_pat_on_ref_mut: false,
        ..Self::ERGO2024
    };

    pub const RPJOHNST: Self = RuleOptions {
        // TODO: double_ref: Last | Min
        allow_ref_pat_on_ref_mut: false,
        ..Self::STATELESS
    };

    /// The known bundles, with a short explanation.
    pub const KNOWN_OPTION_BUNDLES: &[(&str, Self, &str)] = &[
        (
            "default",
            Self::DEFAULT,
            "a reasonable proposal; like `stateless` but forbids `ref` bindings that create temporaries",
        ),
        (
            "stateless",
            Self::STATELESS,
            "a proposal that tracks no hidden state; purely type-based",
        ),
        (
            "stable_rust",
            Self::STABLE_RUST,
            "emulates the behavior of current stable rust",
        ),
        ("ergo2024", Self::ERGO2024, "the accepted RFC3627 behavior"),
        (
            "rfc3627_2021",
            Self::RFC3627_2021,
            "the accepted RFC3627 behavior under edition 2021",
        ),
        (
            "ergo2024_breaking_only",
            Self::ERGO2024_BREAKING_ONLY,
            "the breaking changes for edition 2024 planned in RFC3627",
        ),
        (
            "structural",
            Self::STRUCTURAL,
            "purely structural matching, with no match ergonomics",
        ),
        (
            "waffle",
            Self::WAFFLE,
            "a proposal by @WaffleLapkin (excluding the proposed rule3 extension)",
        ),
        // ("rpjohnst", Self::RPJOHNST, "(TODO) a proposal by @rpjohnst"),
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
}
