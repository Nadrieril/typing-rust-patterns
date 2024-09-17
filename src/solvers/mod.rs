use std::collections::BTreeMap;

use crate::*;

mod analysis;
mod bm_based;
mod ty_based;
mod typing_rules;

pub use analysis::*;
pub use bm_based::*;
pub use ty_based::*;
pub use typing_rules::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingAssignments<'a> {
    pub assignments: BTreeMap<&'a str, Type<'a>>,
}

impl<'a> BindingAssignments<'a> {
    pub fn new(assignments: impl IntoIterator<Item = (&'a str, Type<'a>)>) -> Self {
        Self {
            assignments: assignments.into_iter().collect(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TypingResult<'a> {
    Success(BindingAssignments<'a>),
    BorrowError(BindingAssignments<'a>, BorrowCheckError),
    TypeError(TypeError),
}

impl RuleSet {
    pub fn typecheck<'a>(&self, a: &'a Arenas<'a>, req: &TypingRequest<'a>) -> TypingResult<'a> {
        match *self {
            RuleSet::TypeBased(options) => typecheck_with_this_crate(a, options, req),
            RuleSet::BindingModeBased(conf) => typecheck_with_formality(a, conf, req),
        }
    }

    pub fn trace_solver<'a>(
        &self,
        a: &'a Arenas<'a>,
        req: &TypingRequest<'a>,
        style: PredicateStyle,
    ) -> (String, TypingResult<'a>) {
        match *self {
            RuleSet::TypeBased(options) => trace_solver(a, *req, options, style),
            RuleSet::BindingModeBased(conf) => trace_with_formality(a, conf, req),
        }
    }
}
