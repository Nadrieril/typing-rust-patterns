use crate::*;
use bincode::{Decode, Encode};
use gloo_utils::format::JsValueSerdeExt;
use serde::Serialize;
use std::cmp::Ordering;
use std::collections::BTreeMap;
use std::fmt::Write;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub fn set_panic_hook() {
    console_error_panic_hook::set_once();
}

/// Encode a value as base64.
fn encode_base64<T: Encode>(x: &T) -> String {
    use base64::{engine::general_purpose::URL_SAFE, Engine as _};
    let config = bincode::config::standard();
    let bits = bincode::encode_to_vec(x, config).unwrap();
    URL_SAFE.encode(bits)
}

/// Decode the current style from base64.
fn decode_base64<T: Decode>(x: JsValue) -> Option<T> {
    use base64::{engine::general_purpose::URL_SAFE, Engine as _};
    let config = bincode::config::standard();
    let bits = URL_SAFE.decode(x.as_string()?).ok()?;
    Some(bincode::decode_from_slice(&bits, config).ok()?.0)
}

#[test]
/// Ensure we don't change the encoding of rulesets, as that would break saved links into the app.
fn ruleset_encodings() -> anyhow::Result<()> {
    use std::fmt::Write;
    let mut out = String::new();
    for b in KNOWN_TY_BASED_BUNDLES {
        let ruleset = RuleSetJs {
            this_solver: true,
            ty_based: b.ruleset,
            bm_based: Conf::rfc2005(),
        };
        let _ = writeln!(&mut out, "{}: {}", b.name, ruleset.encode());
    }
    insta::with_settings!({
        snapshot_path => "../tests/snapshots",
        snapshot_suffix => "ruleset-encodings",
        prepend_module_to_snapshot => false,
        omit_expression => true,
    }, {
        insta::assert_snapshot!(out);
    });
    Ok(())
}

/// Like `RuleSet` but remembers the ruleset of the other solver to avoid losing state in the
/// frontend.
#[wasm_bindgen]
#[derive(Debug, Clone, Copy, Encode, Decode)]
pub struct RuleSetJs {
    this_solver: bool,
    ty_based: RuleOptions,
    bm_based: Conf,
}

impl RuleSetJs {
    pub fn as_ruleset(&self) -> RuleSet {
        if self.this_solver {
            RuleSet::TypeBased(self.ty_based)
        } else {
            RuleSet::BindingModeBased(self.bm_based)
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

    pub fn eq(&self, other: &RuleSetJs) -> bool {
        self.as_ruleset() == other.as_ruleset()
    }

    pub fn options_doc(&self) -> Vec<JsValue> {
        if self.this_solver {
            TY_BASED_OPTIONS_DOC
        } else {
            BM_BASED_OPTIONS_DOC
        }
        .iter()
        .map(|x| JsValue::from_serde(x).unwrap())
        .collect()
    }

    pub fn bundles_doc(&self) -> Vec<JsValue> {
        RuleSet::known_rulesets()
            .filter(|b| self.this_solver == b.ruleset.is_ty_based())
            .map(|x| JsValue::from_serde(&x).unwrap())
            .collect()
    }

    pub fn from_bundle_name(ty_name: &str, bm_name: &str) -> Option<RuleSetJs> {
        Some(Self {
            this_solver: true,
            ty_based: RuleOptions::from_bundle_name(ty_name)?,
            bm_based: bm_based_from_bundle_name(bm_name)?,
        })
    }

    pub fn with_bundle_name(&self, name: &str) -> RuleSetJs {
        if self.this_solver {
            Self {
                ty_based: RuleOptions::from_bundle_name(name).unwrap_or(self.ty_based),
                ..*self
            }
        } else {
            Self {
                bm_based: bm_based_from_bundle_name(name).unwrap_or(self.bm_based),
                ..*self
            }
        }
    }

    pub fn get_bundle_name(&self) -> Option<String> {
        self.as_ruleset().get_bundle_name().map(str::to_owned)
    }

    /// List options that can be changed without affecting the current rules.
    pub fn irrelevant_options(&self) -> Vec<String> {
        self.as_ruleset()
            .irrelevant_options()
            .iter()
            .copied()
            .map(str::to_owned)
            .collect()
    }

    pub fn get_key(&self, key: &str) -> String {
        self.as_ruleset().get_key(key)
    }

    pub fn with_key(&self, key: &str, val: &str) -> Self {
        let mut out = *self;
        if self.this_solver {
            out.ty_based.set_key(key, val);
        } else {
            bm_based_set_key(&mut out.bm_based, key, val);
        }
        out
    }

    /// Encode the current options as base64.
    pub fn encode(&self) -> String {
        encode_base64(self)
    }

    /// Decode the current options from base64.
    pub fn decode(x: JsValue) -> Option<RuleSetJs> {
        decode_base64(x)
    }

    /// Runs the solver on this input. Returns the trace of the solver steps and the result of
    /// typechecking.
    pub fn trace_solver(
        &self,
        request: &str,
        &PredicateStyleJs(style): &PredicateStyleJs,
    ) -> Vec<String> {
        let a = &Arenas::default();
        let req = match TypingRequest::parse(a, request) {
            Ok(req) => req,
            Err(e) => return vec![format!("parse error: {e}"), String::new()],
        };
        let (trace, res) = self.as_ruleset().trace_solver(a, &req, style);
        vec![trace.to_string(), res.to_string()]
    }

    pub fn display_rules(&self, &PredicateStyleJs(style): &PredicateStyleJs) -> Vec<String> {
        assert!(self.this_solver);
        let arenas = &Arenas::default();
        let options = self.ty_based;
        let ctx = TypingCtx {
            arenas,
            options,
            type_of_interest: style.type_of_interest(),
        };
        compute_rules(ctx)
            .into_iter()
            .map(|rule| rule.display(style).unwrap())
            .collect()
    }
}

/// Wrapper because wasm_bindgen doesn't support non-trivial enums.
#[wasm_bindgen]
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Encode, Decode)]
pub struct PredicateStyleJs(PredicateStyle);

#[wasm_bindgen]
impl PredicateStyleJs {
    pub fn from_name_and_option(name: &str, ruleset: &RuleSetJs) -> Option<PredicateStyleJs> {
        // Silly but DRY
        Self::from_name_and_options(name, ruleset, ruleset)
    }

    /// Constructs a sequent-style style based on the given name (reflecting the type of interest)
    /// and the passed rulesets (reflecting )
    pub fn from_name_and_options(
        name: &str,
        left: &RuleSetJs,
        right: &RuleSetJs,
    ) -> Option<PredicateStyleJs> {
        let style = if name == "Let" {
            PredicateStyle::Let
        } else {
            let ty = TypeOfInterest::from_str(name).ok()?;
            let left = left.as_ruleset();
            let right = right.as_ruleset();
            PredicateStyle::sequent_with_minimal_state(ty, left, right)
        };
        Some(PredicateStyleJs(style))
    }

    pub fn to_name(&self) -> String {
        match self.0 {
            PredicateStyle::Let => "Let".to_string(),
            PredicateStyle::Sequent { ty, .. } => ty.to_str(),
        }
    }

    pub fn doc(&self) -> String {
        let mut out = String::new();
        match self.0 {
            PredicateStyle::Let => {
                let _ = write!(
                    &mut out,
                    "Track the user-observable type along with the expression being matched on"
                );
            }
            PredicateStyle::Sequent {
                ty: type_of_interest,
                show_reference_state,
                ..
            } => {
                let _ = write!(&mut out, "Track the type ");
                match type_of_interest {
                    TypeOfInterest::UserVisible => {
                        let _ = write!(&mut out, "that the user observes");
                        if show_reference_state {
                            let _ = write!(&mut out, " and whether the outermost reference in the type is real or inherited");
                        }
                    }
                    TypeOfInterest::InMemory => {
                        let _ = write!(&mut out, "of the matched place");
                        if show_reference_state {
                            let _ = write!(&mut out, " and the current binding mode");
                        }
                    }
                }
            }
        }
        out
    }

    pub fn display_generic_predicate(&self) -> String {
        let a = &Arenas::default();
        TypingPredicate {
            pat: &Pattern::ABSTRACT,
            expr: Expression::ABSTRACT.borrow(a, Mutability::Shared),
        }
        .display(self.0)
    }

    pub fn explain_predicate(&self) -> String {
        let explanation = self.0.explain_predicate();
        let mut out = String::new();
        let _ = writeln!(&mut out, "{}, where:", explanation.pred.code());
        let _ = writeln!(&mut out, "<ul>");
        for component in explanation.components {
            let _ = writeln!(&mut out, "<li>{}</li>", component);
        }
        let _ = writeln!(&mut out, "</ul>");
        out
    }
}

#[wasm_bindgen]
pub fn diff_trace_solver_js(
    left: &RuleSetJs,
    right: &RuleSetJs,
    request: &str,
    &PredicateStyleJs(style): &PredicateStyleJs,
) -> Vec<String> {
    let a = &Arenas::default();
    let req = match TypingRequest::parse(a, request) {
        Ok(req) => req,
        Err(e) => return vec![format!("parse error: {e}"), String::new()],
    };
    let (left_trace, left_res) = left.as_ruleset().trace_solver(a, &req, style);
    let (right_trace, right_res) = right.as_ruleset().trace_solver(a, &req, style);
    let (left_trace, right_trace) = left_trace.diff_display(&right_trace);
    let (left_res, right_res) = left_res.display_diffed(&right_res);
    vec![left_trace, right_trace, left_res, right_res]
}

#[wasm_bindgen]
pub fn display_joint_rules_js(
    left: &RuleSetJs,
    right: &RuleSetJs,
    &PredicateStyleJs(style): &PredicateStyleJs,
) -> Vec<JsValue> {
    #[derive(Debug, Clone, Serialize)]
    pub struct JointDisplayOutput {
        left: String,
        right: String,
    }

    assert!(left.this_solver);
    assert!(right.this_solver);
    let a = &Arenas::default();
    let type_of_interest = style.type_of_interest();
    compute_joint_rules(a, type_of_interest, left.ty_based, right.ty_based)
        .into_iter()
        .map(|joint_rule| {
            let (left, right) = joint_rule.as_ref().left_and_right();
            let left = left
                .map(|r| r.display_to_tree(a, style).unwrap())
                .unwrap_or_default();
            let right = right
                .map(|r| r.display_to_tree(a, style).unwrap())
                .unwrap_or_default();
            let (mut left, mut right, has_diff) = left.diff_display_has_diff(&right);
            if !has_diff {
                left = left.dimmed();
                right = right.dimmed();
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
    direction: i8,
) -> Vec<JsValue> {
    #[derive(Debug, Clone, Serialize)]
    pub struct CompareOutput {
        req: String,
        left: String,
        right: String,
        structured: StructuredCompareOutput,
    }
    #[derive(Debug, Clone, Serialize)]
    pub struct StructuredCompareOutput {
        query: StructuredCompareOutputQuery,
        left: StructuredCompareOutputResult,
        right: StructuredCompareOutputResult,
    }
    #[derive(Debug, Clone, Serialize)]
    pub struct StructuredCompareOutputQuery {
        pattern: String,
        #[serde(rename = "type")]
        type_: String,
    }
    #[derive(Debug, Clone, Serialize)]
    #[serde(tag = "kind")]
    pub enum StructuredCompareOutputResult {
        Success {
            bindings: BTreeMap<String, String>,
        },
        BorrowError {
            name: String,
            bindings: BTreeMap<String, String>,
        },
        TypeError {
            name: String,
        },
    }

    assert!(direction.abs() <= 1);
    let direction: Ordering = unsafe { std::mem::transmute(direction) };

    let a = &Arenas::default();
    let bindings_to_strings = |bindings: &BindingAssignments| {
        bindings
            .assignments
            .iter()
            .map(|(var, ty)| (var.to_string(), ty.to_string()))
            .collect()
    };
    let ty_res_to_structured = |res: &TypingResult| match res {
        TypingResult::Success(bindings) => StructuredCompareOutputResult::Success {
            bindings: bindings_to_strings(bindings),
        },
        TypingResult::BorrowError(bindings, e) => StructuredCompareOutputResult::BorrowError {
            name: e.to_string(),
            bindings: bindings_to_strings(bindings),
        },
        TypingResult::TypeError(e) => StructuredCompareOutputResult::TypeError {
            name: e.to_string(),
        },
    };
    compare_rulesets(
        a,
        pat_depth,
        ty_depth,
        left_ruleset.as_ruleset(),
        direction,
        right_ruleset.as_ruleset(),
    )
    .into_iter()
    .map(|(req, left, right)| {
        let structured = StructuredCompareOutput {
            query: StructuredCompareOutputQuery {
                pattern: req.pat.to_string(),
                type_: req.ty.to_string(),
            },
            left: ty_res_to_structured(&left),
            right: ty_res_to_structured(&right),
        };
        let (left, right) = left.display_diffed(&right);
        let req = req.to_string();
        CompareOutput {
            req,
            left,
            right,
            structured,
        }
    })
    .map(|out| JsValue::from_serde(&out).unwrap())
    .collect()
}
