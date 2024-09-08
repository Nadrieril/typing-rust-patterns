use crate::*;
use gloo_utils::format::JsValueSerdeExt;
use serde::Serialize;
use wasm_bindgen::prelude::*;

#[derive(Serialize)]
pub struct OptionsDoc {
    pub name: &'static str,
    pub values: &'static [&'static str],
    pub doc: &'static str,
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
            .copied()
            .map(|(name, values, doc)| OptionsDoc { name, values, doc })
            .map(|options| JsValue::from_serde(&options).unwrap())
            .collect()
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
