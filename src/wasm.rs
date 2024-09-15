use crate::*;
use bincode::{Decode, Encode};
use gloo_utils::format::JsValueSerdeExt;
use match_ergonomics_formality::Conf;
use printer::Style;
use serde::Serialize;
use std::cmp::Ordering;
use std::fmt::Write;
use wasm_bindgen::prelude::*;

const ERGO_FORMALITY_OPTIONS_DOC: &[OptionsDoc] = &[
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

/// Like `RuleSet` but remembers the ruleset of the other solver to avoid losing state in the
/// frontend.
#[wasm_bindgen]
#[derive(Debug, Clone, Copy, Encode, Decode)]
pub struct RuleSetJs {
    this_solver: bool,
    ty_based: RuleOptions,
    bm_based: Conf,
}

#[derive(Debug, Clone, Serialize)]
pub struct BundleDocJs {
    pub name: &'static str,
    #[serde(skip)]
    pub ruleset: RuleSet,
    pub doc: &'static str,
}

impl RuleSetJs {
    pub fn as_ruleset(&self) -> RuleSet {
        if self.this_solver {
            RuleSet::TypeBased(self.ty_based)
        } else {
            RuleSet::BindingModeBased(self.bm_based)
        }
    }

    pub fn bundles_doc(&self) -> Vec<BundleDocJs> {
        if self.this_solver {
            RuleOptions::KNOWN_OPTION_BUNDLES
                .iter()
                .map(|bundle| BundleDocJs {
                    name: bundle.name,
                    ruleset: RuleSet::TypeBased(bundle.options),
                    doc: bundle.doc,
                })
                .collect()
        } else {
            vec![
                BundleDocJs {
                    name: "pre_rfc2005",
                    doc: "Disable match ergonomics",
                    ruleset: RuleSet::BindingModeBased(Conf::pre_rfc2005()),
                },
                BundleDocJs {
                    name: "stable",
                    doc: "Stable rust behavior",
                    ruleset: RuleSet::BindingModeBased(Conf::rfc2005()),
                },
                BundleDocJs {
                    name: "rfc3627",
                    doc: "RFC3627 behavior on edition 2024",
                    ruleset: RuleSet::BindingModeBased(Conf::rfc3627_2024()),
                },
                BundleDocJs {
                    name: "rfc3627_2021",
                    doc: "RFC3627 behavior on edition 2021",
                    ruleset: RuleSet::BindingModeBased(Conf::rfc3627_2021()),
                },
                BundleDocJs {
                    name: "rfc3627_2024_min",
                    doc: "Initial breaking changes to do over the edition for RFC3627",
                    ruleset: RuleSet::BindingModeBased(Conf::rfc_3627_2024_min()),
                },
                BundleDocJs {
                    name: "waffle",
                    doc: "A proposal by @WaffleLapkin",
                    ruleset: RuleSet::BindingModeBased(Conf::waffle_2024()),
                },
                BundleDocJs {
                    name: "rpjohnst",
                    doc: "A proposal by @rpjohnst",
                    ruleset: RuleSet::BindingModeBased(Conf::rpjohnst_2024()),
                },
            ]
        }
    }
}

#[wasm_bindgen]
impl RuleSetJs {
    pub fn get_solver(&self) -> bool {
        self.this_solver
    }

    pub fn with_solver(&self, this_solver: bool) -> RuleSetJs {
        Self {
            this_solver,
            ..*self
        }
    }

    pub fn options_doc(&self) -> Vec<JsValue> {
        if self.this_solver {
            RuleOptions::OPTIONS_DOC
        } else {
            ERGO_FORMALITY_OPTIONS_DOC
        }
        .iter()
        .map(|x| JsValue::from_serde(x).unwrap())
        .collect()
    }

    pub fn bundles_doc_js(&self) -> Vec<JsValue> {
        self.bundles_doc()
            .into_iter()
            .map(|x| JsValue::from_serde(&x).unwrap())
            .collect()
    }

    pub fn from_bundle_name_js(ty_name: &str, bm_name: &str) -> Option<RuleSetJs> {
        Some(Self {
            this_solver: true,
            ty_based: RuleOptions::from_bundle_name(ty_name)?,
            bm_based: {
                let mut conf = Conf::default();
                conf.set(bm_name).unwrap();
                conf
            },
        })
    }

    pub fn with_bundle_name(&self, name: &str) -> RuleSetJs {
        if self.this_solver {
            Self {
                ty_based: RuleOptions::from_bundle_name(name).unwrap(),
                ..*self
            }
        } else {
            match self.bundles_doc().into_iter().find(|b| b.name == name) {
                Some(b) => match b.ruleset {
                    RuleSet::BindingModeBased(bm_based) => Self { bm_based, ..*self },
                    RuleSet::TypeBased(_) => unreachable!(),
                },
                None => *self,
            }
        }
    }

    pub fn get_bundle_name_js(&self) -> Option<String> {
        let this_ruleset = self.as_ruleset();
        self.bundles_doc()
            .into_iter()
            .find(|b| b.ruleset == this_ruleset)
            .map(|b| b.name.to_owned())
    }

    /// List options that can be changed without affecting the current rules.
    pub fn irrelevant_options_js(&self) -> Vec<String> {
        if self.this_solver {
            self.ty_based.irrelevant_options()
        } else if self.bm_based.no_me {
            &["rule1", "rule2", "rule3", "rule4", "rule5", "spin"] as &[_]
        } else {
            &[]
        }
        .iter()
        .copied()
        .map(str::to_owned)
        .collect()
    }

    pub fn get_key(&self, key: &str) -> String {
        if self.this_solver {
            serde_yaml::to_string(&self.ty_based.to_map()[key])
                .unwrap()
                .trim()
                .to_string()
        } else {
            // Mutable copy to reuse upstream `get_mut`.
            let mut conf = self.bm_based;
            match key {
                "no_me" | "rule1" | "rule2" | "rule5" | "spin" => {
                    if *conf.get_mut(key).unwrap() {
                        "true"
                    } else {
                        "false"
                    }
                }
                "rule3" => {
                    if self.bm_based.rule3_lazy {
                        "rule3_lazy"
                    } else if self.bm_based.rule3 {
                        if self.bm_based.rule3_ext1 {
                            "rule3_ext1"
                        } else {
                            "rule3"
                        }
                    } else {
                        "false"
                    }
                }
                "rule4" => {
                    if self.bm_based.rule4_early {
                        "rule4_early"
                    } else if self.bm_based.rule4 {
                        if self.bm_based.rule4_ext {
                            "rule4_ext"
                        } else if self.bm_based.rule4_ext2 {
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
            .to_owned()
        }
    }

    pub fn with_key(&self, key: &str, val: &str) -> Self {
        let mut out = *self;
        if self.this_solver {
            out.ty_based.set_key(key, val);
        } else {
            match key {
                "no_me" | "rule1" | "rule2" | "rule5" | "spin" => {
                    *out.bm_based.get_mut(key).unwrap() = val == "true";
                }
                "rule3" => {
                    // Reset all variants before setting the one we want to avoid inconsistencies.
                    out.bm_based.rule3 = false;
                    out.bm_based.rule3_ext1 = false;
                    out.bm_based.rule3_lazy = false;
                    match val {
                        "false" => {}
                        "rule3" => out.bm_based.rule3 = true,
                        "rule3_ext1" => {
                            out.bm_based.rule3 = true;
                            out.bm_based.rule3_ext1 = true;
                        }
                        "rule3_lazy" => {
                            out.bm_based.rule3_lazy = true;
                        }
                        _ => panic!("Unknown option: {key}"),
                    }
                }
                "rule4" => {
                    // Reset all variants before setting the one we want to avoid inconsistencies.
                    out.bm_based.rule4 = false;
                    out.bm_based.rule4_ext = false;
                    out.bm_based.rule4_ext2 = false;
                    out.bm_based.rule4_early = false;
                    match val {
                        "false" => {}
                        "rule4" => out.bm_based.rule4 = true,
                        "rule4_ext" => {
                            out.bm_based.rule4 = true;
                            out.bm_based.rule4_ext = true;
                        }
                        "rule4_ext2" => {
                            out.bm_based.rule4 = true;
                            out.bm_based.rule4_ext2 = true;
                        }
                        "rule4_early" => {
                            out.bm_based.rule4_early = true;
                        }
                        _ => panic!("Unknown option: {key}"),
                    }
                }
                _ => panic!("Unknown option: {key}"),
            }
            .to_owned()
        }
        out
    }

    /// Encode the current options as base64.
    pub fn encode(&self) -> String {
        use base64::{engine::general_purpose::URL_SAFE, Engine as _};
        let config = bincode::config::standard();
        let bits = bincode::encode_to_vec(self, config).unwrap();
        URL_SAFE.encode(bits)
    }

    /// Decode the current options from base64.
    pub fn decode(x: JsValue) -> Option<RuleSetJs> {
        use base64::{engine::general_purpose::URL_SAFE, Engine as _};
        let config = bincode::config::standard();
        let bits = URL_SAFE.decode(x.as_string()?).ok()?;
        Some(bincode::decode_from_slice(&bits, config).ok()?.0)
    }
}

#[wasm_bindgen]
// `wasm_bindgen` doesn't support methods on enums: https://github.com/rustwasm/wasm-bindgen/issues/1715
pub fn style_from_name(name: &str) -> PredicateStyle {
    serde_yaml::from_str(name).unwrap()
}

#[wasm_bindgen]
pub fn explain_predicate_js(style: PredicateStyle) -> String {
    let explanation = style.explain_predicate();
    let mut out = String::new();
    let _ = writeln!(&mut out, "{}, where:", explanation.pred.code());
    let _ = writeln!(&mut out, "<ul>");
    for component in explanation.components {
        let _ = writeln!(&mut out, "<li>{}</li>", component);
    }
    let _ = writeln!(&mut out, "</ul>");
    out
}

#[wasm_bindgen]
pub fn trace_solver_js(request: &str, ruleset: &RuleSetJs, style: PredicateStyle) -> String {
    let a = &Arenas::default();
    let req = match TypingRequest::parse(a, request) {
        Ok(req) => req,
        Err(e) => return format!("parse error: {e}"),
    };
    match ruleset.as_ruleset() {
        RuleSet::TypeBased(options) => {
            let options = RuleOptions {
                always_inspect_bm: matches!(style, PredicateStyle::SequentBindingMode),
                ..options
            };
            trace_solver(req, options, style)
        }
        RuleSet::BindingModeBased(conf) => trace_with_formality(conf, &req),
    }
}

#[wasm_bindgen]
pub fn display_rules_js(ruleset: &RuleSetJs, style: PredicateStyle) -> String {
    assert!(ruleset.this_solver);
    let options = ruleset.ty_based;
    let options = RuleOptions {
        always_inspect_bm: matches!(style, PredicateStyle::SequentBindingMode),
        ..options
    };
    display_rules(style, options).unwrap()
}

#[wasm_bindgen]
pub fn display_joint_rules_js(
    left: &RuleSetJs,
    right: &RuleSetJs,
    style: PredicateStyle,
) -> Vec<JsValue> {
    #[derive(Debug, Clone, Serialize)]
    pub struct JointDisplayOutput {
        left: String,
        right: String,
    }

    assert!(left.this_solver);
    assert!(right.this_solver);
    let always_inspect_bm = matches!(style, PredicateStyle::SequentBindingMode);
    let left = RuleOptions {
        always_inspect_bm,
        ..left.ty_based
    };
    let right = RuleOptions {
        always_inspect_bm,
        ..right.ty_based
    };

    let arenas = &Arenas::default();
    compute_joint_rules(arenas, left, right)
        .into_iter()
        .map(|joint_rule| {
            let (left, right) = joint_rule.left_and_right();
            let mut left = left.map(|r| r.display(style).unwrap()).unwrap_or_default();
            let mut right = right.map(|r| r.display(style).unwrap()).unwrap_or_default();
            if left != right {
                left = left.red();
                right = right.green();
            }
            JointDisplayOutput { left, right }
        })
        .map(|out| JsValue::from_serde(&out).unwrap())
        .collect()
}

#[wasm_bindgen]
pub fn compare_rulesets_js(
    left_ruleset: &RuleSetJs,
    right_ruleset: &RuleSetJs,
    pat_depth: usize,
    ty_depth: usize,
) -> Vec<JsValue> {
    #[derive(Debug, Clone, Serialize)]
    pub struct CompareOutput {
        req: String,
        left: String,
        right: String,
    }

    let a = &Arenas::default();
    compare_rulesets(
        a,
        pat_depth,
        ty_depth,
        left_ruleset.as_ruleset(),
        Ordering::Equal,
        right_ruleset.as_ruleset(),
    )
    .into_iter()
    .map(|(req, left, right)| CompareOutput {
        req: req.to_string(),
        left: left.to_string(),
        right: right.to_string(),
    })
    .map(|out| JsValue::from_serde(&out).unwrap())
    .collect()
}
