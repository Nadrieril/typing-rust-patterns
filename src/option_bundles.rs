use crate::*;

impl RuleOptions {
    /// Reproduces stable rust behavior.
    pub const STABLE_RUST: Self = RuleOptions {
        rules_display_style: TypingRuleStyle::Plain,
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
        rules_display_style: TypingRuleStyle::Plain,
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
        rules_display_style: TypingRuleStyle::Plain,
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
}
