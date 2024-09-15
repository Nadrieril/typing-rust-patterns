use std::collections::VecDeque;
use std::fmt;
use std::fmt::Write;

use itertools::Itertools;

use crate::*;

#[derive(Clone, Copy)]
pub struct TypingCtx<'a> {
    pub options: RuleOptions,
    pub arenas: &'a Arenas<'a>,
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

    pub fn display_state(&self, style: PredicateStyle) -> impl fmt::Display + '_ {
        self.done_predicates
            .iter()
            .map(|p| p.display_as_let())
            .chain(self.predicates.iter().map(|p| p.display(style)))
            .join("\n")
    }

    pub fn display_final_state(
        &self,
        ctx: TypingCtx<'a>,
        _style: PredicateStyle,
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
pub fn trace_solver<'a>(
    request: TypingRequest<'a>,
    options: RuleOptions,
    style: PredicateStyle,
) -> String {
    let arenas = &Arenas::default();
    let ctx = TypingCtx { arenas, options };
    let mut solver = TypingSolver::new(request);
    let mut trace = String::new();
    let _ = write!(&mut trace, "{}\n", solver.display_state(style));
    loop {
        match solver.step(ctx) {
            Ok(rule) => {
                let line = format!("// Applying rule `{}`", rule.display(options));
                let _ = write!(&mut trace, "{}\n", line.comment());
                let _ = write!(&mut trace, "{}\n", solver.display_state(style));
            }
            Err(e) => {
                match e {
                    CantStep::Done => {
                        let _ = write!(&mut trace, "\n// Final bindings (simplified):\n");
                        let _ = write!(&mut trace, "{}\n", solver.display_final_state(ctx, style));
                    }
                    CantStep::NoApplicableRule(pred, err) => {
                        let line = format!("// Type error for `{}`: {err:?}", pred.display(style));
                        let _ = write!(&mut trace, "{}\n", line.red());
                    }
                }
                break;
            }
        }
    }
    trace
}

pub fn typecheck_with_this_crate<'a>(
    a: &'a Arenas<'a>,
    options: RuleOptions,
    req: &TypingRequest<'a>,
) -> TypingResult<'a> {
    let ctx = TypingCtx { arenas: a, options };
    let mut solver = TypingSolver::new(*req);
    // TODO: abstract over the repeated stepping of the solver
    let e = loop {
        match solver.step(ctx) {
            Ok(_) => {}
            Err(e) => break e,
        }
    };
    match e {
        CantStep::Done => {
            assert_eq!(solver.done_predicates.len(), 1);
            let pred = solver.done_predicates[0];
            let ty = *pred.expr.ty;
            match pred.expr.simplify(ctx).borrow_check() {
                // This error isn't handled by `match-ergo-formality` so we ignore it.
                Ok(()) | Err(BorrowCheckError::CantCopyNestedRefMut) => TypingResult::Success(ty),
                Err(err) => TypingResult::BorrowError(ty, err),
            }
        }
        CantStep::NoApplicableRule(_, err) => TypingResult::TypeError(err),
    }
}
