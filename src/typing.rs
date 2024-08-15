use core::fmt;
use std::collections::VecDeque;

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
        match (*self.pat, *self.expr.ty) {
            // Constructor rules
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
            (Pattern::Tuple(_), Type::Ref(_, Type::Tuple(_))) => {
                Err(CantStep::NoApplicableRule(self.clone()))
            }

            // Dereference rules
            (Pattern::Ref(p_mtbl, p_inner), Type::Ref(t_mtbl, _)) => match (p_mtbl, t_mtbl) {
                (Mutable::No, Mutable::No) => Ok((
                    Rule::Deref,
                    vec![TypingPredicate {
                        pat: p_inner,
                        expr: self.expr.deref(ctx.arenas),
                    }],
                )),

                _ => todo!("{self}"),
            },

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

            _ => todo!("{self}"),
        }
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
            .chain(self.predicates.iter())
            .format(", ")
    }
}

#[test]
fn test_step_solver() {
    let arenas = &Arenas::default();
    let test_steps: &[(&str, &[&str])] = &[
        ("&x: &T", &["x @ *p: T"]),
        ("ref x: T", &["x @ &p: &T"]),
        ("ref mut x: T", &["x @ &mut p: &mut T"]),
        ("mut ref mut x: T", &["mut x @ &mut p: &mut T"]),
        ("[x]: &[T]", &["x @ &(*p).0: &T"]),
        ("[x, y]: &[T, U]", &["x @ &(*p).0: &T, y @ &(*p).1: &U"]),
        (
            "[x, &y]: &[T, U]",
            &[
                "x @ &(*p).0: &T, &y @ &(*p).1: &U",
                "x @ &(*p).0: &T, y @ *&(*p).1: U",
            ],
        ),
        (
            "[ref x]: &[T]",
            &["ref x @ &(*p).0: &T", "x @ &&(*p).0: &&T"],
        ),
    ];
    for (request, mut expected_steps) in test_steps {
        let ctx = TypingCtx {
            arenas,
            options: RuleOptions {
                ref_on_expr: RefOnExprBehavior::AllocTemporary,
            },
        };
        let request = complete_parse_typing_request(&arenas, request)
            .map_err(|e| e.to_string())
            .unwrap();
        let mut solver = TypingSolver::new(request);
        loop {
            match solver.step(ctx) {
                Ok(_rule) => {
                    let state_str = solver.display_state().to_string();
                    let [this_step, rest @ ..] = expected_steps else {
                        panic!("unexpected next step: {state_str}")
                    };
                    expected_steps = rest;
                    assert_eq!(*this_step, state_str);
                }
                Err(CantStep::Done) => break,
                Err(CantStep::NoApplicableRule(pred)) => panic!("no rule applies to {pred}"),
            }
        }
    }
}
