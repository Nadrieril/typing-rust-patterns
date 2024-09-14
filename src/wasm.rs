use crate::*;
use gloo_utils::format::JsValueSerdeExt;
use serde::Serialize;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(getter_with_clone)]
#[derive(Debug, Clone)]
pub struct BundleDocJs {
    pub name: String,
    pub options: RuleOptions,
    pub doc: String,
}

#[wasm_bindgen]
impl RuleOptions {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        RuleOptions::DEFAULT
    }

    pub fn options_doc() -> Vec<JsValue> {
        Self::OPTIONS_DOC
            .iter()
            .map(|options| JsValue::from_serde(options).unwrap())
            .collect()
    }

    pub fn bundles_doc() -> Vec<BundleDocJs> {
        Self::KNOWN_OPTION_BUNDLES
            .iter()
            .map(|bundle| BundleDocJs {
                name: bundle.name.to_string(),
                options: bundle.options,
                doc: bundle.doc.to_string(),
            })
            .collect()
    }

    /// List options that can be changed without affecting the current rules.
    pub fn irrelevant_options_js(&self) -> Vec<String> {
        self.irrelevant_options()
            .iter()
            .copied()
            .map(str::to_owned)
            .collect()
    }

    pub fn get_key(&self, key: &str) -> String {
        serde_yaml::to_string(&self.to_map()[key])
            .unwrap()
            .trim()
            .to_string()
    }

    pub fn with_key(&self, key: &str, val: &str) -> Self {
        let mut out = *self;
        out.set_key(key, val);
        out
    }

    pub fn to_js(&self) -> JsValue {
        JsValue::from_serde(&self).unwrap()
    }

    pub fn get_bundle_name_js(&self) -> Option<String> {
        self.get_bundle_name().map(String::from)
    }
}

#[wasm_bindgen]
// `wasm_bindgen` doesn't support methods on enums: https://github.com/rustwasm/wasm-bindgen/issues/1715
pub fn style_from_name(name: &str) -> PredicateStyle {
    serde_yaml::from_str(name).unwrap()
}

#[wasm_bindgen]
pub fn trace_solver_js(request: &str, options: &RuleOptions, style: PredicateStyle) -> String {
    let a = &Arenas::default();
    let options = RuleOptions {
        always_inspect_bm: matches!(style, PredicateStyle::SequentBindingMode),
        ..*options
    };
    match TypingRequest::parse(a, request) {
        Ok(req) => trace_solver(req, options, style),
        Err(e) => format!("parse error: {e}"),
    }
}

#[wasm_bindgen]
pub fn display_rules_js(options: &RuleOptions, style: PredicateStyle) -> String {
    let options = RuleOptions {
        always_inspect_bm: matches!(style, PredicateStyle::SequentBindingMode),
        ..*options
    };
    display_rules(style, options).unwrap()
}

#[derive(Debug, Clone, Serialize)]
pub struct JointDisplayOutput {
    pub left: String,
    pub right: String,
}

#[wasm_bindgen]
pub fn display_joint_rules_js(
    left: &RuleOptions,
    right: &RuleOptions,
    style: PredicateStyle,
) -> Vec<JsValue> {
    let always_inspect_bm = matches!(style, PredicateStyle::SequentBindingMode);
    let left = RuleOptions {
        always_inspect_bm,
        ..*left
    };
    let right = RuleOptions {
        always_inspect_bm,
        ..*right
    };

    let arenas = &Arenas::default();
    compute_joint_rules(arenas, left, right)
        .into_iter()
        .map(|joint_rule| {
            let (left, right) = joint_rule.left_and_right();
            let left = left.map(|r| r.display(style).unwrap()).unwrap_or_default();
            let right = right.map(|r| r.display(style).unwrap()).unwrap_or_default();
            JointDisplayOutput { left, right }
        })
        .map(|out| JsValue::from_serde(&out).unwrap())
        .collect()
}
