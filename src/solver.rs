use std::collections::VecDeque;
use std::fmt;

use itertools::Itertools;

use crate::*;

#[derive(Clone, Copy)]
pub struct TypingCtx<'a> {
    pub options: RuleOptions,
    pub arenas: &'a Arenas<'a>,
}

/// The inner state of our solver: the typing of `let pat: type = expr`. We write it `pat @ expr :
/// type`.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypingPredicate<'a> {
    pub pat: &'a Pattern<'a>,
    pub expr: Expression<'a>,
}

pub enum CantStep<'a> {
    Done,
    NoApplicableRule(TypingPredicate<'a>, TypeError),
}

/// The solver itself. It contains a set of predicates to satisfy
#[derive(Clone)]
pub struct TypingSolver<'a> {
    pub predicates: VecDeque<TypingPredicate<'a>>,
    pub done_predicates: Vec<TypingPredicate<'a>>,
}

impl<'a> TypingSolver<'a> {
    pub fn new(req: TypingRequest<'a>) -> Self {
        let pred = TypingPredicate::new(req);
        TypingSolver {
            predicates: [pred].into(),
            done_predicates: Vec::new(),
        }
    }

    /// Run one step of solving.
    pub fn step(&mut self, ctx: TypingCtx<'a>) -> Result<Rule, CantStep<'a>> {
        // Queue-like fashion: we always process the first one first.
        let Some(first_pred) = self.predicates.pop_front() else {
            return Err(CantStep::Done);
        };
        match first_pred.step(ctx) {
            Ok((rule, new_preds)) => {
                if new_preds.is_empty() {
                    self.done_predicates.push(first_pred);
                } else {
                    for p in new_preds.into_iter().rev() {
                        self.predicates.push_front(p);
                    }
                }
                Ok(rule)
            }
            Err(err) => Err(CantStep::NoApplicableRule(first_pred, err)),
        }
    }

    pub fn display_state(&self, style: TypingRuleStyle) -> impl fmt::Display + '_ {
        self.done_predicates
            .iter()
            .map(|p| p.display_as_let())
            .chain(self.predicates.iter().map(|p| p.display(style)))
            .join("\n")
    }

    pub fn display_final_state(
        &self,
        ctx: TypingCtx<'a>,
        _style: TypingRuleStyle,
    ) -> impl fmt::Display + '_ {
        assert!(self.predicates.is_empty());
        self.done_predicates
            .iter()
            .map(|p| {
                let p = p.simplify_expr(ctx);
                let bck = p.expr.borrow_check();
                let bck = bck
                    .err()
                    .map(|err| format!(" // Borrow-check error: {err:?}"))
                    .unwrap_or_default();
                let p = p.display_as_let();
                format!("{p};{bck}")
            })
            .join("\n")
    }
}

/// Run the solver on this request and returns the trace as a string.
pub fn trace_solver(
    request: &str,
    options: RuleOptions,
    style: TypingRuleStyle,
) -> anyhow::Result<String> {
    use std::fmt::Write;
    let arenas = &Arenas::default();
    let ctx = TypingCtx { arenas, options };
    let request = TypingRequest::parse(&arenas, request)?;
    let mut solver = TypingSolver::new(request);
    let mut trace = String::new();
    let _ = write!(&mut trace, "{}\n", solver.display_state(style));
    loop {
        match solver.step(ctx) {
            Ok(rule) => {
                let _ = write!(&mut trace, "// Applying rule `{}`\n", rule.display(options));
                let _ = write!(&mut trace, "{}\n", solver.display_state(style));
            }
            Err(e) => {
                match e {
                    CantStep::Done => {
                        let _ = write!(&mut trace, "\n// Final bindings (simplified):\n");
                        let _ = write!(&mut trace, "{}\n", solver.display_final_state(ctx, style));
                    }
                    CantStep::NoApplicableRule(pred, err) => {
                        let _ = write!(
                            &mut trace,
                            "// Type error for `{}`: {err:?}\n",
                            pred.display(style)
                        );
                    }
                }
                break;
            }
        }
    }
    Ok(trace)
}
