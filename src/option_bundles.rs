use crate::*;

impl RuleOptions {
    /// Reproduces stable rust behavior.
    pub const STABLE_RUST: Self = RuleOptions {
        rules_display_style: TypingRuleStyle::Plain,
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::ResetBindingMode,
        mut_binding_on_inherited: MutBindingOnInheritedBehavior::ResetBindingMode,
        inherited_ref_on_ref: InheritedRefOnRefBehavior::EatBoth,
        allow_ref_pat_on_ref_mut: false,
        simplify_deref_mut: true,
        eat_inherited_ref_alone: false,
        downgrade_mut_inside_shared: false,
    };

    /// Reproduces RFC3627 (match ergonomics 2024) behavior
    pub const ERGO2024: Self = RuleOptions {
        rules_display_style: TypingRuleStyle::Plain,
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::ResetBindingMode,
        mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
        inherited_ref_on_ref: InheritedRefOnRefBehavior::EatInner,
        allow_ref_pat_on_ref_mut: true,
        simplify_deref_mut: true,
        eat_inherited_ref_alone: true,
        downgrade_mut_inside_shared: true,
    };

    /// A fairly permissive proposal.
    pub const PERMISSIVE: Self = RuleOptions {
        rules_display_style: TypingRuleStyle::Plain,
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::AllocTemporary,
        mut_binding_on_inherited: MutBindingOnInheritedBehavior::Keep,
        inherited_ref_on_ref: InheritedRefOnRefBehavior::EatOuter,
        allow_ref_pat_on_ref_mut: true,
        simplify_deref_mut: true,
        eat_inherited_ref_alone: true,
        downgrade_mut_inside_shared: false,
    };

    /// A fairly permissive proposal, with the benefit of requiring 0 implicit state: we never
    /// inspect the DBM, we only follow the types.
    pub const STATELESS: Self = RuleOptions {
        simplify_deref_mut: false,
        ..Self::PERMISSIVE
    };

    /// The default setting for the solver. A reasonable proposal.
    pub const DEFAULT: Self = RuleOptions {
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::ResetBindingMode,
        ..Self::PERMISSIVE
    };

    /// A restrictive set of rules that accepts very little. Used mainly for tests.
    pub const RESTRICTIVE: Self = RuleOptions {
        rules_display_style: TypingRuleStyle::Plain,
        ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
        mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
        inherited_ref_on_ref: InheritedRefOnRefBehavior::EatOuter,
        allow_ref_pat_on_ref_mut: false,
        simplify_deref_mut: false,
        eat_inherited_ref_alone: false,
        downgrade_mut_inside_shared: false,
    };

    pub const WAFFLE: Self = RuleOptions {
        inherited_ref_on_ref: InheritedRefOnRefBehavior::EatOuter,
        allow_ref_pat_on_ref_mut: false,
        ..Self::ERGO2024
    };

    /// The known bundles, with a short explanation.
    pub const KNOWN_OPTION_BUNDLES: &[(&str, Self, &str)] = &[
        (
            "default",
            Self::DEFAULT,
            "the default settings; a reasonable proposal",
        ),
        (
            "permissive",
            Self::PERMISSIVE,
            "an even more permissive proposal than the default",
        ),
        // TODO: add "no match ergonomics" ruleset
        // TODO: rename "bundle" to "ruleset"
        (
            "restrictive",
            Self::RESTRICTIVE,
            "a restrictive set of rules that accepts very little",
        ),
        (
            "stateless",
            Self::STATELESS,
            "a proposal that tracks no hidden state; purely type-based",
        ),
        ("ergo2024", Self::ERGO2024, "wip emulation of RFC3627 rules"),
        ("waffle", Self::WAFFLE, "a proposal by @WaffleLapkin"),
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
