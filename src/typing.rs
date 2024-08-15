use std::collections::VecDeque;
use std::fmt;

use itertools::Itertools;

use crate::*;

/// What to do to a `ref x` binding to an `&p` or `&mut p` expression (as opposed to an inner place
/// of the scrutinee).
#[derive(Clone, Copy)]
pub enum RefOnExprBehavior {
    /// Borrow that expression, which requires allocating a temporary variable.
    AllocTemporary,
    /// Stable rust behavior: skip the borrow in the expression and re-borrow the inner.
    ResetBindingMode,
    /// Treat this as an error.
    Error,
}

/// Choice of typing rules.
#[derive(Clone, Copy)]
pub struct RuleOptions {
    pub ref_on_expr: RefOnExprBehavior,
    pub allow_ref_pat_on_ref_mut: bool,
    pub simplify_expressions: bool,
}

#[derive(Clone, Copy)]
pub struct TypingCtx<'a> {
    pub options: RuleOptions,
    pub arenas: &'a Arenas<'a>,
}

/// The inner state of our solver: the typing of `let pat: type = expr`. We write it `pat @ expr :
/// type`.
#[derive(Clone)]
pub struct TypingPredicate<'a> {
    pub pat: &'a Pattern<'a>,
    pub expr: Expression<'a>,
}

/// The various typing rules we can apply.
#[derive(Debug, Clone, Copy)]
pub enum Rule {
    Constructor,
    ConstructorRef,
    Deref,
    Binding,
    ExprSimplification,
}

pub enum CantStep<'a> {
    Done,
    NoApplicableRule(TypingPredicate<'a>),
}

impl<'a> TypingPredicate<'a> {
    pub fn new(req: TypingRequest<'a>) -> Self {
        TypingPredicate {
            pat: req.pat,
            expr: Expression {
                kind: ExprKind::Scrutinee,
                ty: req.ty,
            },
        }
    }

    /// Apply one step of rule to this predicate.
    pub fn step(&self, ctx: TypingCtx<'a>) -> Result<(Rule, Vec<Self>), CantStep<'a>> {
        if ctx.options.simplify_expressions {
            // Expression simplification rules.
            match self.expr.kind {
                ExprKind::CastAsImmRef(Expression {
                    kind: ExprKind::Ref(Mutable::Yes, e),
                    ..
                }) => {
                    return Ok((
                        Rule::ExprSimplification,
                        vec![TypingPredicate {
                            pat: self.pat,
                            expr: e.borrow(ctx.arenas, Mutable::No),
                        }],
                    ))
                }
                ExprKind::Deref(Expression {
                    kind: ExprKind::Ref(Mutable::Yes, e),
                    ..
                }) => {
                    return Ok((
                        Rule::ExprSimplification,
                        vec![TypingPredicate {
                            pat: self.pat,
                            expr: **e,
                        }],
                    ))
                }
                _ => {}
            }
        }

        match (*self.pat, *self.expr.ty) {
            // Constructor rules
            (Pattern::Tuple(pats), Type::Tuple(tys)) if pats.len() == tys.len() => {
                let preds = pats
                    .iter()
                    .enumerate()
                    .map(|(i, pat)| {
                        let expr = self.expr.field(ctx.arenas, i);
                        TypingPredicate { pat, expr }
                    })
                    .collect();
                Ok((Rule::Constructor, preds))
            }
            (Pattern::Tuple(pats), Type::Ref(t_mtbl, Type::Tuple(tys)))
                if pats.len() == tys.len() =>
            {
                let preds = pats
                    .iter()
                    .enumerate()
                    .map(|(i, pat)| {
                        let expr = self
                            .expr
                            .deref(ctx.arenas)
                            .field(ctx.arenas, i)
                            .borrow(ctx.arenas, t_mtbl);
                        TypingPredicate { pat, expr }
                    })
                    .collect();
                Ok((Rule::ConstructorRef, preds))
            }
            (Pattern::Tuple(_), _) => Err(CantStep::NoApplicableRule(self.clone())),

            // Dereference rules
            (Pattern::Ref(p_mtbl, p_inner), Type::Ref(t_mtbl, _)) => match (p_mtbl, t_mtbl) {
                (Mutable::No, Mutable::No) | (Mutable::Yes, Mutable::Yes) => Ok((
                    Rule::Deref,
                    vec![TypingPredicate {
                        pat: p_inner,
                        expr: self.expr.deref(ctx.arenas),
                    }],
                )),
                (Mutable::No, Mutable::Yes) if ctx.options.allow_ref_pat_on_ref_mut => Ok((
                    Rule::Deref,
                    vec![TypingPredicate {
                        pat: self.pat,
                        expr: self.expr.cast_as_imm_ref(ctx.arenas),
                    }],
                )),
                (Mutable::No, Mutable::Yes) | (Mutable::Yes, Mutable::No) => {
                    Err(CantStep::NoApplicableRule(self.clone()))
                }
            },
            (Pattern::Ref(..), _) => Err(CantStep::NoApplicableRule(self.clone())),

            // Binding rules
            (Pattern::Binding(mtbl, BindingMode::ByRef(by_ref_mtbl), name), _) => {
                // To replicate stable rust behavior, we must inspect the place so we don't
                // re-borrow
                match (self.expr.binding_mode(), ctx.options.ref_on_expr) {
                    (BindingMode::ByMove, _) | (_, RefOnExprBehavior::AllocTemporary) => Ok((
                        Rule::Binding,
                        vec![TypingPredicate {
                            pat: Pattern::Binding(mtbl, BindingMode::ByMove, name)
                                .alloc(ctx.arenas),
                            expr: self.expr.borrow(ctx.arenas, by_ref_mtbl),
                        }],
                    )),
                    (BindingMode::ByRef(_), RefOnExprBehavior::ResetBindingMode) => {
                        todo!()
                    }
                    (BindingMode::ByRef(_), RefOnExprBehavior::Error) => {
                        Err(CantStep::NoApplicableRule(self.clone()))
                    }
                }
            }
            (Pattern::Binding(_, BindingMode::ByMove, _), _) => Err(CantStep::Done),
        }
    }

    /// Whether this predicate is completed, i.e. is a simple binding pattern.
    pub fn is_done(&self) -> bool {
        matches!(self.pat, Pattern::Binding(_, BindingMode::ByMove, _))
    }
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
                for p in new_preds.into_iter().rev() {
                    self.predicates.push_front(p);
                }
                Ok(rule)
            }
            Err(CantStep::Done) => {
                self.done_predicates.push(first_pred);
                self.step(ctx)
            }
            Err(CantStep::NoApplicableRule(pred)) => Err(CantStep::NoApplicableRule(pred)),
        }
    }

    pub fn display_state(&self) -> impl fmt::Display + '_ {
        self.done_predicates
            .iter()
            .map(|p| p.display_done().to_string())
            .chain(self.predicates.iter().map(|p| format!("{p}")))
            .join("\n")
    }
}

/// Run the solver on this request and returns the trace as a string.
pub fn trace_solver(request: &str, options: RuleOptions) -> String {
    use std::fmt::Write;
    let arenas = &Arenas::default();
    let ctx = TypingCtx { arenas, options };
    let request = complete_parse_typing_request(&arenas, request)
        .map_err(|e| e.to_string())
        .unwrap();
    let mut solver = TypingSolver::new(request);
    let mut trace = String::new();
    let _ = write!(&mut trace, "Query: `{request}`\n\n");
    let _ = write!(&mut trace, "{}\n", solver.display_state());
    loop {
        match solver.step(ctx) {
            Ok(rule) => {
                let _ = write!(&mut trace, "// Applying rule `{rule:?}`\n");
                let _ = write!(&mut trace, "{}\n", solver.display_state());
            }
            Err(e) => {
                match e {
                    CantStep::Done => {
                        let _ = write!(&mut trace, "\n// Final bindings:\n");
                        let _ = write!(&mut trace, "{}\n", solver.display_state());
                    }
                    CantStep::NoApplicableRule(pred) => {
                        let _ = write!(&mut trace, "// ERROR: no rules applies to {pred}\n");
                    }
                }
                break;
            }
        }
    }
    trace
}
