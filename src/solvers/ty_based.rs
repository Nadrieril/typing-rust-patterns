use std::collections::VecDeque;

use crate::*;

#[derive(Clone, Copy)]
pub struct TypingCtx<'a> {
    pub arenas: &'a Arenas<'a>,
    pub options: RuleOptions,
    /// If the type of interest is the in-memory one, we'll always inspect the binding mode when
    /// computing rules.
    pub type_of_interest: TypeOfInterest,
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

    pub fn display_state(&self, ctx: TypingCtx<'a>, style: PredicateStyle) -> DisplayTree<'a> {
        let a = ctx.arenas;
        DisplayTree::sep_by(
            a,
            "\n",
            self.done_predicates
                .iter()
                .map(|p| p.display_as_let(a))
                .chain(self.predicates.iter().map(|p| p.display_to_tree(a, style))),
        )
    }

    pub fn display_final_state(
        &self,
        ctx: TypingCtx<'a>,
        _style: PredicateStyle,
    ) -> DisplayTree<'a> {
        let a = ctx.arenas;
        assert!(self.predicates.is_empty());
        DisplayTree::sep_by(
            a,
            "\n",
            self.done_predicates.iter().map(|p| {
                let p = p.simplify_expr(ctx);
                let bck = p.expr.borrow_check();
                let bck = bck
                    .err()
                    .map(|err| format!(" // Borrow-check error: {err:?}"))
                    .unwrap_or_default();
                let p = p.display_as_let(a);
                p.sep_then(a, ";", bck)
            }),
        )
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
    a: &'a Arenas<'a>,
    request: TypingRequest<'a>,
    options: RuleOptions,
    style: PredicateStyle,
) -> (DisplayTree<'a>, TypingResult<'a>) {
    let ctx = TypingCtx {
        arenas: a,
        options,
        type_of_interest: TypeOfInterest::UserVisible,
    };
    let mut trace = Vec::new();
    let mut final_state = None;
    let res = run_solver(ctx, &request, |solver, event| match event {
        SolverTraceEvent::Start => {
            trace.push(solver.display_state(ctx, style));
        }
        SolverTraceEvent::Step(rule) => {
            let line = format!("// Applying rule `{}`", rule.display(ctx.options));
            trace.push(line.comment().to_display_tree(a).ignore_for_diff());
            trace.push(solver.display_state(ctx, style));
        }
        SolverTraceEvent::CantStep(e) => match e {
            CantStep::Done => {
                final_state = Some(
                    "\n// Final bindings (simplified):"
                        .comment()
                        .to_display_tree(a)
                        .sep_then(a, "\n", solver.display_final_state(ctx, style))
                        .then(a, "\n"),
                );
            }
            CantStep::NoApplicableRule(pred, err) => {
                let line = format!("// Type error for `{}`: {err:?}", pred.display(style));
                trace.push(line.red().to_display_tree(a).ignore_for_diff());
            }
        },
    });
    let trace = DisplayTree::sep_by_compare_prefix(a, "\n", trace);
    let final_state = final_state.unwrap_or_default();
    let out = trace.sep_then(a, "\n", final_state);
    (out, res)
}

pub fn typecheck_with_this_crate<'a>(
    arenas: &'a Arenas<'a>,
    options: RuleOptions,
    req: &TypingRequest<'a>,
) -> TypingResult<'a> {
    let ctx = TypingCtx {
        arenas,
        options,
        type_of_interest: TypeOfInterest::UserVisible,
    };
    run_solver(ctx, req, |_, _| {})
}
