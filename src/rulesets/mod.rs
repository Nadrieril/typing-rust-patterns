use serde::Serialize;

use crate::*;

mod bm_based;
mod ty_based;

pub use bm_based::*;
pub use ty_based::*;

#[derive(Debug, Clone, Copy, Serialize)]
pub struct OptionValue {
    pub name: &'static str,
    pub doc: &'static str,
}

#[derive(Debug, Clone, Copy, Serialize)]
pub struct OptionsDoc {
    /// Name of the option.
    pub name: &'static str,
    /// Description of the option.
    pub doc: &'static str,
    /// Names and descriptions of the possible values.
    pub values: &'static [OptionValue],
}

#[derive(Debug, Clone, Copy, Serialize)]
pub struct BundleDoc<T> {
    pub name: &'static str,
    #[serde(skip)]
    pub ruleset: T,
    pub doc: &'static str,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RuleSet {
    TypeBased(RuleOptions),
    BindingModeBased(Conf),
}

impl RuleSet {
    pub fn is_ty_based(&self) -> bool {
        matches!(self, Self::TypeBased(..))
    }

    pub fn is_bm_based(&self) -> bool {
        matches!(self, Self::BindingModeBased(..))
    }

    pub fn as_ty_based(&self) -> Option<&RuleOptions> {
        if let Self::TypeBased(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn as_bm_based(&self) -> Option<&Conf> {
        if let Self::BindingModeBased(v) = self {
            Some(v)
        } else {
            None
        }
    }

    pub fn get_key(&self, key: &str) -> String {
        match self {
            RuleSet::TypeBased(o) => o.get_key(key),
            RuleSet::BindingModeBased(c) => bm_based_get_key(*c, key).to_owned(),
        }
    }

    /// List options that can be changed without affecting the current rules.
    pub fn irrelevant_options(&self) -> &'static [&'static str] {
        match self {
            RuleSet::TypeBased(o) => o.irrelevant_options(),
            RuleSet::BindingModeBased(c) => bm_based_irrelevant_options(*c),
        }
    }

    /// Whether this ruleset cares about scrutinee mutability.
    pub fn tracks_scrut_mutability(&self) -> bool {
        match self {
            RuleSet::TypeBased(opts) => opts.tracks_scrut_mutability(),
            RuleSet::BindingModeBased(_) => true,
        }
    }

    /// Whether this ruleset tracks some state related to inherited references/binding mode.
    pub fn tracks_reference_state(&self, ty: TypeOfInterest) -> bool {
        match self {
            RuleSet::TypeBased(opts) => opts.tracks_reference_state(ty),
            RuleSet::BindingModeBased(_) => true,
        }
    }

    pub fn known_rulesets() -> impl Iterator<Item = BundleDoc<Self>> {
        KNOWN_TY_BASED_BUNDLES
            .iter()
            .map(|bundle| BundleDoc {
                name: bundle.name,
                ruleset: RuleSet::TypeBased(bundle.ruleset),
                doc: bundle.doc,
            })
            .chain(KNOWN_BM_BASED_BUNDLES.iter().map(|bundle| BundleDoc {
                name: bundle.name,
                ruleset: RuleSet::BindingModeBased(bundle.ruleset),
                doc: bundle.doc,
            }))
    }

    pub fn get_bundle_name(self) -> Option<&'static str> {
        Self::known_rulesets()
            .find(|b| b.ruleset == self)
            .map(|b| b.name)
    }
}
