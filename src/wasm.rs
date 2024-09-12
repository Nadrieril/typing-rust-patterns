use crate::*;
use gloo_utils::format::JsValueSerdeExt;
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
pub fn trace_solver_str(request: &str, options: &RuleOptions) -> String {
    let a = &Arenas::default();
    match TypingRequest::parse(a, request) {
        Ok(req) => trace_solver(req, *options, PredicateStyle::Sequent),
        Err(e) => format!("parse error: {e}"),
    }
}
