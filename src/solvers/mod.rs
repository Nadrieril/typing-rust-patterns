use crate::*;

mod analysis;
mod bm_based;
mod ty_based;
mod typing_rules;

pub use analysis::*;
pub use bm_based::*;
pub use ty_based::*;
pub use typing_rules::*;

#[derive(Debug, Clone, Copy)]
pub enum TypingResult<'a> {
    Success(Type<'a>),
    BorrowError(Type<'a>, BorrowCheckError),
    TypeError(TypeError),
}

impl RuleSet {
    pub fn typecheck<'a>(&self, a: &'a Arenas<'a>, req: &TypingRequest<'a>) -> TypingResult<'a> {
        match *self {
            RuleSet::TypeBased(options) => typecheck_with_this_crate(a, options, req),
            RuleSet::BindingModeBased(conf) => typecheck_with_formality(a, conf, req),
        }
    }

    pub fn trace_solver(&self, req: &TypingRequest<'_>, style: PredicateStyle) -> String {
        match *self {
            RuleSet::TypeBased(options) => {
                let options = RuleOptions {
                    always_inspect_bm: matches!(style, PredicateStyle::SequentBindingMode),
                    ..options
                };
                trace_solver(*req, options, style)
            }
            RuleSet::BindingModeBased(conf) => trace_with_formality(conf, req),
        }
    }
}
