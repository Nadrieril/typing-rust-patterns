use std::collections::VecDeque;
use std::fmt;
use std::fmt::Write;

use itertools::Itertools;

use crate::*;

#[derive(Clone, Copy)]
pub struct TypingCtx<'a> {
    pub arenas: &'a Arenas<'a>,
    pub options: RuleOptions,
    /// Whether to always branch on the binding mode when computing rules. This is required for the
    /// `SequentBindingMode` style
    pub always_inspect_bm: bool,
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

pub enum SolverTraceEvent<'a, 'b> {
    Start,
    Step(Rule),
    CantStep(&'b CantStep<'a>),
}

/// Run the solver on this request and return the result of typechecking.
pub fn run_solver<'a>(
    ctx: TypingCtx<'a>,
    request: &TypingRequest<'a>,
    mut callback: impl FnMut(&TypingSolver<'a>, SolverTraceEvent<'a, '_>),
) -> TypingResult<'a> {
    let mut solver = TypingSolver::new(*request);
    callback(&solver, SolverTraceEvent::Start);
    let e = loop {
        match solver.step(ctx) {
            Ok(rule) => callback(&solver, SolverTraceEvent::Step(rule)),
            Err(e) => {
                callback(&solver, SolverTraceEvent::CantStep(&e));
                break e;
            }
        }
    };

    match e {
        CantStep::Done => {
            let bindings = BindingAssignments::new(solver.done_predicates.iter().map(|pred| {
                let ty = *pred.expr.ty;
                let Pattern::Binding(_, _, name) = pred.pat else {
                    unreachable!()
                };
                (*name, ty)
            }));
            let borrow_check: Result<(), _> = solver
                .done_predicates
                .iter()
                .map(|pred| pred.expr.simplify(ctx).borrow_check())
                .collect();
            match borrow_check {
                Ok(()) => TypingResult::Success(bindings),
                Err(err) => TypingResult::BorrowError(bindings, err),
            }
        }
        CantStep::NoApplicableRule(_, err) => TypingResult::TypeError(err),
    }
}

/// Run the solver on this request and returns the trace as a string.
pub fn trace_solver<'a>(
    request: TypingRequest<'a>,
    options: RuleOptions,
    style: PredicateStyle,
) -> String {
    let arenas = &Arenas::default();
    let ctx = TypingCtx {
        arenas,
        options,
        always_inspect_bm: false,
    };
    let mut trace = String::new();
    run_solver(ctx, &request, |solver, event| match event {
        SolverTraceEvent::Start => {
            let _ = write!(&mut trace, "{}\n", solver.display_state(style));
        }
        SolverTraceEvent::Step(rule) => {
            let line = format!("// Applying rule `{}`", rule.display(ctx.options));
            let _ = write!(&mut trace, "{}\n", line.comment());
            let _ = write!(&mut trace, "{}\n", solver.display_state(style));
        }
        SolverTraceEvent::CantStep(e) => match e {
            CantStep::Done => {
                let _ = write!(&mut trace, "\n// Final bindings (simplified):\n");
                let _ = write!(&mut trace, "{}\n", solver.display_final_state(ctx, style));
            }
            CantStep::NoApplicableRule(pred, err) => {
                let line = format!("// Type error for `{}`: {err:?}", pred.display(style));
                let _ = write!(&mut trace, "{}\n", line.red());
            }
        },
    });
    trace
}

pub fn typecheck_with_this_crate<'a>(
    arenas: &'a Arenas<'a>,
    options: RuleOptions,
    req: &TypingRequest<'a>,
) -> TypingResult<'a> {
    let ctx = TypingCtx {
        arenas,
        options,
        always_inspect_bm: false,
    };
    run_solver(ctx, req, |_, _| {})
}
