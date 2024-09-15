use std::sync::LazyLock;

use crate::*;

pub const BM_BASED_OPTIONS_DOC: &[OptionsDoc] = &[
    OptionsDoc {
        name: "no_me",
        doc: "TODO",
        values: &[
            OptionValue {
                name: "false",
                doc: "Disable match ergonomics entirely",
            },
            OptionValue {
                name: "true",
                doc: "Disable match ergonomics entirely",
            },
        ],
    },
    OptionsDoc {
        name: "rule1",
        doc: "TODO",
        values: &[
            OptionValue {
                name: "false",
                doc: "When the default binding mode is not `move`, writing `mut` on a \
                    binding is an error",
            },
            OptionValue {
                name: "true",
                doc: "When the default binding mode is not `move`, writing `mut` on a \
                    binding is an error",
            },
        ],
    },
    OptionsDoc {
        name: "rule2",
        doc: "TODO",
        values: &[
            OptionValue {
                name: "false",
                doc: "When a reference pattern matches against a reference, do not \
                    update the default binding mode",
            },
            OptionValue {
                name: "true",
                doc: "When a reference pattern matches against a reference, do not \
                    update the default binding mode",
            },
        ],
    },
    OptionsDoc {
        name: "rule3",
        doc: "TODO",
        values: &[
            OptionValue {
                name: "false",
                doc: "Keep track of whether we have matched either a reference pattern \
                    or a non-reference pattern against a shared reference, and if \
                    so, set the DBM to `ref` when we would otherwise set it to `ref \
                    mut`",
            },
            OptionValue {
                name: "rule3",
                doc: "Keep track of whether we have matched either a reference pattern \
                    or a non-reference pattern against a shared reference, and if \
                    so, set the DBM to `ref` when we would otherwise set it to `ref \
                    mut`",
            },
            OptionValue {
                name: "rule3_ext1",
                doc: "If we've previously matched against a shared reference in the \
                    scrutinee (or against a `ref` DBM under Rule 4, or against a \
                    mutable reference treated as a shared one or a `ref mut` DB \
                    treated as a `ref` one under Rule 5), if we've reached a binding \
                    and the scrutinee is a mutable reference, coerce it to a shared \
                    reference",
            },
            OptionValue {
                name: "rule3_lazy",
                doc: "Rule 3, but lazily applied",
            },
        ],
    },
    OptionsDoc {
        name: "rule4",
        doc: "TODO",
        values: &[
            OptionValue {
                name: "false",
                doc: "If a reference pattern is being matched against a non-reference \
                    type and if the DBM is `ref` or `ref mut`, match the pattern \
                    against the DBM as though it were a type",
            },
            OptionValue {
                name: "rule4",
                doc: "If a reference pattern is being matched against a non-reference \
                    type and if the DBM is `ref` or `ref mut`, match the pattern \
                    against the DBM as though it were a type",
            },
            OptionValue {
                name: "rule4_ext",
                doc: "If an `&` pattern is being matched against a non-reference type \
                    or an `&mut` pattern is being matched against a shared reference \
                    type or a non-reference type, and if the DBM is `ref` or `ref \
                    mut`, match the pattern against the DBM as though it were a \
                    type",
            },
            OptionValue {
                name: "rule4_ext2",
                doc: "If an `&` pattern is being matched against a mutable reference \
                    type or a non-reference type, or if an `&mut` pattern is being \
                    matched against a shared reference type or a non-reference type, \
                    and if the DBM is `ref` or `ref mut`, match the pattern against \
                    the DBM as though it were a type",
            },
            OptionValue {
                name: "rule4_early",
                doc: "If the DBM is `ref` or `ref mut`, match a reference pattern \
                    against it as though it were a type *before* considering the \
                    scrutinee.",
            },
        ],
    },
    OptionsDoc {
        name: "rule5",
        doc: "TODO",
        values: &[
            OptionValue {
                name: "false",
                doc: "If a `&` pattern is being matched against a type of mutable \
                    reference (or against a `ref mut` DBM under *Rule 4*), act as \
                    though the type were a shared reference instead (or that a `ref \
                    mut` DBM is a `ref` DBM instead)",
            },
            OptionValue {
                name: "true",
                doc: "If a `&` pattern is being matched against a type of mutable \
                    reference (or against a `ref mut` DBM under *Rule 4*), act as \
                    though the type were a shared reference instead (or that a `ref \
                    mut` DBM is a `ref` DBM instead)",
            },
        ],
    },
    OptionsDoc {
        name: "spin",
        doc: "TODO",
        values: &[
            OptionValue {
                name: "false",
                doc: "Spin rule",
            },
            OptionValue {
                name: "true",
                doc: "Spin rule",
            },
        ],
    },
];

/// List options that can be changed without affecting the current rules.
pub fn bm_based_irrelevant_options(conf: Conf) -> &'static [&'static str] {
    if conf.no_me {
        &["rule1", "rule2", "rule3", "rule4", "rule5", "spin"]
    } else {
        &[]
    }
}

pub fn bm_based_get_key(conf: Conf, key: &str) -> &'static str {
    // Mutable copy to reuse upstream `get_mut`.
    let mut conf = conf;
    match key {
        "no_me" | "rule1" | "rule2" | "rule5" | "spin" => {
            if *conf.get_mut(key).unwrap() {
                "true"
            } else {
                "false"
            }
        }
        "rule3" => {
            if conf.rule3_lazy {
                "rule3_lazy"
            } else if conf.rule3 {
                if conf.rule3_ext1 {
                    "rule3_ext1"
                } else {
                    "rule3"
                }
            } else {
                "false"
            }
        }
        "rule4" => {
            if conf.rule4_early {
                "rule4_early"
            } else if conf.rule4 {
                if conf.rule4_ext {
                    "rule4_ext"
                } else if conf.rule4_ext2 {
                    "rule4_ext2"
                } else {
                    "rule4"
                }
            } else {
                "false"
            }
        }
        _ => {
            panic!("Unknown option: {key}")
        }
    }
}

pub fn bm_based_set_key(conf: &mut Conf, key: &str, val: &str) {
    match key {
        "no_me" | "rule1" | "rule2" | "rule5" | "spin" => {
            *conf.get_mut(key).unwrap() = val == "true";
        }
        "rule3" => {
            // Reset all variants before setting the one we want to avoid inconsistencies.
            conf.rule3 = false;
            conf.rule3_ext1 = false;
            conf.rule3_lazy = false;
            match val {
                "false" => {}
                "rule3" => conf.rule3 = true,
                "rule3_ext1" => {
                    conf.rule3 = true;
                    conf.rule3_ext1 = true;
                }
                "rule3_lazy" => {
                    conf.rule3_lazy = true;
                }
                _ => panic!("Unknown option: {key}"),
            }
        }
        "rule4" => {
            // Reset all variants before setting the one we want to avoid inconsistencies.
            conf.rule4 = false;
            conf.rule4_ext = false;
            conf.rule4_ext2 = false;
            conf.rule4_early = false;
            match val {
                "false" => {}
                "rule4" => conf.rule4 = true,
                "rule4_ext" => {
                    conf.rule4 = true;
                    conf.rule4_ext = true;
                }
                "rule4_ext2" => {
                    conf.rule4 = true;
                    conf.rule4_ext2 = true;
                }
                "rule4_early" => {
                    conf.rule4_early = true;
                }
                _ => panic!("Unknown option: {key}"),
            }
        }
        _ => panic!("Unknown option: {key}"),
    }
}

/// The known bundles, with a short explanation.
pub static KNOWN_BM_BASED_BUNDLES: LazyLock<Vec<BundleDoc<Conf>>> = LazyLock::new(|| {
    vec![
        BundleDoc {
            name: "pre_rfc2005",
            doc: "Disable match ergonomics",
            ruleset: Conf::pre_rfc2005(),
        },
        BundleDoc {
            name: "stable",
            doc: "Stable rust behavior",
            ruleset: Conf::rfc2005(),
        },
        BundleDoc {
            name: "rfc3627",
            doc: "RFC3627 behavior on edition 2024",
            ruleset: Conf::rfc3627_2024(),
        },
        BundleDoc {
            name: "rfc3627_2021",
            doc: "RFC3627 behavior on edition 2021",
            ruleset: Conf::rfc3627_2021(),
        },
        BundleDoc {
            name: "rfc3627_2024_min",
            doc: "Initial breaking changes to do over the edition for RFC3627",
            ruleset: Conf::rfc_3627_2024_min(),
        },
        BundleDoc {
            name: "waffle",
            doc: "A proposal by @WaffleLapkin",
            ruleset: Conf::waffle_2024(),
        },
        BundleDoc {
            name: "rpjohnst",
            doc: "A proposal by @rpjohnst",
            ruleset: Conf::rpjohnst_2024(),
        },
    ]
});

pub fn bm_based_from_bundle_name(name: &str) -> Option<Conf> {
    KNOWN_BM_BASED_BUNDLES
        .iter()
        .find(|b| b.name == name)
        .map(|b| b.ruleset)
}
