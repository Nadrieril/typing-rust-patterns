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

    pub const RPJOHNST: Self = RuleOptions {
        // TODO: double_ref: Last | Min
        allow_ref_pat_on_ref_mut: false,
        ..Self::STATELESS
    };
}

#[derive(Debug, Clone, Copy)]
pub struct BundleDoc {
    pub name: &'static str,
    pub options: RuleOptions,
    pub doc: &'static str,
}

impl RuleOptions {
    /// The known bundles, with a short explanation.
    pub const KNOWN_OPTION_BUNDLES: &[BundleDoc] = &[
        BundleDoc {
            name: "nadri",
            options: Self::NADRI,
            doc: "A reasonable proposal; like `stateless` but \
                forbids `ref` bindings that create temporaries",
        },
        BundleDoc {
            name: "stateless",
            options: Self::STATELESS,
            doc: "A proposal that tracks no hidden state; purely type-based",
        },
        BundleDoc {
            name: "stable_rust",
            options: Self::STABLE_RUST,
            doc: "The behavior of current stable rust",
        },
        BundleDoc {
            name: "rfc3627",
            options: Self::ERGO2024,
            doc: "The accepted RFC3627 behavior",
        },
        BundleDoc {
            name: "rfc3627_2021",
            options: Self::RFC3627_2021,
            doc: "The accepted RFC3627 behavior under edition 2021",
        },
        BundleDoc {
            name: "rfc3627_2024_min",
            options: Self::ERGO2024_BREAKING_ONLY,
            doc: "The breaking changes for edition 2024 planned in RFC3627",
        },
        BundleDoc {
            name: "structural",
            options: Self::STRUCTURAL,
            doc: "Purely structural matching, with no match ergonomics",
        },
        BundleDoc {
            name: "waffle",
            options: Self::WAFFLE,
            doc: "A proposal by @WaffleLapkin (excluding the proposed rule3 extension)",
        },
        // ("rpjohnst", Self::RPJOHNST, "(TODO) a proposal by @rpjohnst"),
    ];

    pub fn get_bundle_name(self) -> Option<&'static str> {
        Self::KNOWN_OPTION_BUNDLES
            .iter()
            .find(|b| b.options == self)
            .map(|b| b.name)
    }

    pub fn from_bundle_name(name: &str) -> Option<Self> {
        Self::KNOWN_OPTION_BUNDLES
            .iter()
            .find(|b| b.name == name)
            .map(|b| b.options)
    }
}
