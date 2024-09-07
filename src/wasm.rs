use wasm_bindgen::prelude::wasm_bindgen;

use crate::*;

#[wasm_bindgen]
pub fn trace_solver_str(request: &str) {
    let options = RuleOptions::DEFAULT;
    let a = &Arenas::default();
    let req = TypingRequest::parse(a, request).unwrap();
    let trace = trace_solver(req, options, PredicateStyle::Sequent);
    web_sys::console::log_1(&trace.into());
}
