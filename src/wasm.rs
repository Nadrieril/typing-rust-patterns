use wasm_bindgen::prelude::wasm_bindgen;

use crate::*;

#[wasm_bindgen]
pub fn trace_solver_str(request: &str) -> String {
    let options = RuleOptions::DEFAULT;
    let a = &Arenas::default();
    let req = TypingRequest::parse(a, request).unwrap();
    trace_solver(req, options, PredicateStyle::Sequent)
}
