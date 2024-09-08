use crate::*;
// use gloo_utils::format::JsValueSerdeExt;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct OptionsDoc {
    #[wasm_bindgen(readonly)]
    pub name: &'static str,
    #[wasm_bindgen(readonly, getter_with_clone)]
    pub values: Vec<String>,
    #[wasm_bindgen(readonly)]
    pub doc: &'static str,
}

#[wasm_bindgen]
impl RuleOptions {
    #[wasm_bindgen(constructor)]
    pub fn new() -> Self {
        RuleOptions::DEFAULT
    }

    pub fn options_doc(&self) -> Vec<OptionsDoc> {
        Self::OPTIONS_DOC
            .iter()
            .copied()
            .map(|(name, values, doc)| OptionsDoc {
                name,
                values: values.iter().copied().map(str::to_owned).collect(),
                doc,
            })
            .collect()
    }
}

#[wasm_bindgen]
pub fn trace_solver_str(request: &str, options: &RuleOptions) -> String {
    let a = &Arenas::default();
    let req = TypingRequest::parse(a, request).unwrap();
    trace_solver(req, *options, PredicateStyle::Sequent)
}
