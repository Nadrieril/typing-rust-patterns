use crate::*;
use bincode::{Decode, Encode};
use gloo_utils::format::JsValueSerdeExt;
use itertools::EitherOrBoth;
use serde::Serialize;
use std::cmp::Ordering;
use std::fmt::Write;
use wasm_bindgen::prelude::*;

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
        vec![trace, res.to_string()]
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
        let ty = TypeOfInterest::from_str(name).ok()?;
        let left = left.as_ruleset();
        let right = right.as_ruleset();
        let style = PredicateStyle::Sequent {
            ty,
            show_reference_state: left.tracks_reference_state(ty)
                || right.tracks_reference_state(ty),
            show_scrut_access: left.tracks_scrut_mutability() || right.tracks_scrut_mutability(),
        };
        Some(PredicateStyleJs(style))
    }

    pub fn to_name(&self) -> String {
        self.0.type_of_interest().to_str()
    }

    pub fn doc(&self) -> String {
        let mut out = String::new();
        match self.0 {
            PredicateStyle::Expression => {
                todo!("explain expression style")
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
            let joint_rule = joint_rule
                .as_ref()
                .map_left(|r| r.make_renderable(a, style).unwrap())
                .map_right(|r| r.make_renderable(a, style).unwrap());
            let (left, right) = match joint_rule {
                EitherOrBoth::Left(left) => {
                    let left = left.display(style).red();
                    (left, String::new())
                }
                EitherOrBoth::Right(right) => {
                    let right = right.display(style).green();
                    (String::new(), right)
                }
                EitherOrBoth::Both(left, right) => {
                    let [mut lpreconditions_str, lbar, lname, lpostconditions_str] =
                        left.display_piecewise(style);
                    let [mut rpreconditions_str, rbar, rname, rpostconditions_str] =
                        right.display_piecewise(style);
                    if lpreconditions_str != rpreconditions_str {
                        lpreconditions_str = lpreconditions_str.red();
                        rpreconditions_str = rpreconditions_str.green();
                    }
                    assert_eq!(lpostconditions_str, rpostconditions_str);
                    let left = RenderableTypingRule::assemble_pieces([
                        lpreconditions_str,
                        lbar,
                        lname,
                        lpostconditions_str,
                    ]);
                    let right = RenderableTypingRule::assemble_pieces([
                        rpreconditions_str,
                        rbar,
                        rname,
                        rpostconditions_str,
                    ]);
                    (left, right)
                }
            };
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
    }

    assert!(direction.abs() <= 1);
    let direction: Ordering = unsafe { std::mem::transmute(direction) };

    let a = &Arenas::default();
    compare_rulesets(
        a,
        pat_depth,
        ty_depth,
        left_ruleset.as_ruleset(),
        direction,
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
