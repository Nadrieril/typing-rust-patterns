use std::fmt::Write;
use std::{cmp::max, fmt::Display};

use itertools::Itertools;

use crate::*;
use BindingMode::*;
use Mutability::*;

impl<'a> TypingPredicate<'a> {
    /// Compute the typing rule associated with this predicate.
    fn typing_rule(&self, ctx: TypingCtx<'a>) -> Result<TypingRule<'a>, TypeError> {
        let (rule, preconditions) = self.step(ctx)?;
        Ok(TypingRule {
            name: rule,
            preconditions,
            postcondition: *self,
        })
    }
}

impl<'a> Pattern<'a> {
    /// Replace abstract subpatterns with all the possible more-precise patterns.
    fn deepen(&'a self, a: &'a Arenas<'a>) -> &'a [Self] {
        match *self {
            Pattern::Abstract(name) => {
                let tuple = {
                    // We assume no rules depend on the specific constructor. We use length 2 for
                    // demo purposes.
                    let subnames = [name.to_string() + "0", name.to_string() + "1"];
                    let subpats = subnames
                        .into_iter()
                        .map(|name| Pattern::Abstract(a.str_arena.alloc_str(&name)));
                    Pattern::Tuple(a.pat_arena.alloc_extend(subpats))
                };
                a.pat_arena.alloc_extend(
                    [tuple]
                        .into_iter()
                        .chain(
                            Mutability::ALL
                                .into_iter()
                                .map(|mtbl| Pattern::Ref(mtbl, self)),
                        )
                        .chain(
                            Mutability::ALL
                                .into_iter()
                                .map(|mtbl| Pattern::Binding(mtbl, ByMove, "x")),
                        )
                        .chain(
                            Mutability::ALL
                                .into_iter()
                                .map(|mtbl| Pattern::Binding(Shared, ByRef(mtbl), "x")),
                        ),
                )
            }
            Pattern::Tuple(pats) => a.pat_arena.alloc_extend(
                pats.iter()
                    .map(|p| p.deepen(a))
                    .multi_cartesian_product()
                    .map(|pats| {
                        Pattern::Tuple(a.pat_arena.alloc_extend(pats.into_iter().copied()))
                    }),
            ),
            Pattern::Ref(mtbl, p) => a
                .pat_arena
                .alloc_extend(p.deepen(a).iter().map(|p| Pattern::Ref(mtbl, p))),
            Pattern::Binding(_, _, _) => std::slice::from_ref(self),
        }
    }
}

impl<'a> Type<'a> {
    /// Replace abstract subtypes with all the possible more-precise types.
    fn deepen(&'a self, a: &'a Arenas<'a>) -> &'a [Self] {
        match *self {
            Type::Var(name) => {
                let tuple = {
                    // We assume no rules depend on the specific type beyond references. We use
                    // length 2 for demo purposes.
                    let subnames = [name.to_string() + "0", name.to_string() + "1"];
                    let subtypes = subnames
                        .into_iter()
                        .map(|name| Type::Var(a.str_arena.alloc_str(&name)));
                    Type::Tuple(a.type_arena.alloc_extend(subtypes))
                };
                a.type_arena.alloc_extend([
                    tuple,
                    Type::Ref(Shared, self),
                    Type::Ref(Mutable, self),
                ])
            }
            Type::Tuple(pats) => a.type_arena.alloc_extend(
                pats.iter()
                    .map(|p| p.deepen(a))
                    .multi_cartesian_product()
                    .map(|tys| Type::Tuple(a.type_arena.alloc_extend(tys.into_iter().copied()))),
            ),
            Type::Ref(mtbl, p) => a
                .type_arena
                .alloc_extend(p.deepen(a).iter().map(|p| Type::Ref(mtbl, p))),
        }
    }
}

impl<'a> Expression<'a> {
    /// Replace abstract subexpressions with all the possible more-precise expressions.
    fn deepen(&self, a: &'a Arenas<'a>) -> &'a [Self] {
        match self.kind {
            ExprKind::Scrutinee => a.expr_arena.alloc_extend([*self]),
            ExprKind::Abstract { bm_is_move: false } => a.expr_arena.alloc_extend([
                Expression {
                    kind: ExprKind::Abstract { bm_is_move: true },
                    ty: self.ty,
                },
                self.borrow(a, Shared),
                self.borrow(a, Mutable),
            ]),
            ExprKind::Abstract { bm_is_move: true } => todo!(),
            ExprKind::Ref(mtbl, e) => a
                .expr_arena
                .alloc_extend(e.deepen(a).iter().map(|e| e.borrow(a, mtbl))),
            // We never generate these.
            ExprKind::Deref(_) | ExprKind::Field(_, _) | ExprKind::CastAsImmRef(_) => {
                unreachable!()
            }
        }
    }

    /// Replace abstract subtypes with all the possible more-precise types.
    fn deepen_ty(&self, a: &'a Arenas<'a>) -> &'a [Self] {
        match self.kind {
            ExprKind::Scrutinee | ExprKind::Abstract { .. } => {
                a.expr_arena
                    .alloc_extend(self.ty.deepen(a).iter().map(|ty| Expression {
                        kind: self.kind,
                        ty,
                    }))
            }
            ExprKind::Ref(mtbl, e) => a
                .expr_arena
                .alloc_extend(e.deepen_ty(a).iter().map(|e| e.borrow(a, mtbl))),
            // We never generate these.
            ExprKind::Deref(_) | ExprKind::Field(_, _) | ExprKind::CastAsImmRef(_) => {
                unreachable!()
            }
        }
    }
}

/// Compute all the rules that describe the behavior of the solver with the given options. We start
/// with a dummy predicate with abstract pattern, expression and type, and recursively refine it as
/// long as we get `OverlyGeneral` errors. This ensures we explore all possible cases.
///
/// Notable exceptions are the "ExprSimplification" rules and `downgrade_shared_inside_shared`
/// option, which don't trigger `OverlyGeneral` errors despite the fact that they inspect the
/// expression. For the simplification rules, the alternative would be overly detailed rules
/// (enough to be disjoint, this would entail specializing every single rule for all possible
/// expressions until depth 2 :')). For `downgrade_shared_inside_shared`, it's not possible to
/// describe it as a rule without tracking aditional state.
fn compute_rules<'a>(ctx: TypingCtx<'a>) -> Vec<TypingRule<'a>> {
    let a = ctx.arenas;
    let mut predicates = vec![TypingPredicate {
        pat: &Pattern::Abstract("p"),
        expr: Expression {
            kind: ExprKind::Abstract { bm_is_move: false },
            ty: Type::Var("T").alloc(a),
        },
    }];

    // Add the special expression simplification predicates because we won't explore them since
    // they don't trigger `OverlyGeneralExpr` errors.
    if ctx.options.simplify_expressions {
        let req = TypingRequest::parse(a, "p: T").unwrap();
        let mut_borrow = &Expression {
            kind: ExprKind::Scrutinee,
            ty: req.ty,
        }
        .borrow(a, Mutable);
        predicates.push(TypingPredicate {
            pat: req.pat,
            expr: mut_borrow.cast_as_imm_ref(a),
        });
        predicates.push(TypingPredicate {
            pat: req.pat,
            expr: mut_borrow.deref(a),
        });
    }

    let mut rules = Vec::new();
    while let Some(pred) = predicates.pop() {
        match pred.typing_rule(ctx) {
            Ok(rule) => rules.push(rule),
            // TODO: try substituting
            Err(TypeError::OverlyGeneralPattern) => predicates.extend(
                pred.pat
                    .deepen(a)
                    .iter()
                    .map(|pat| TypingPredicate { pat, ..pred }),
            ),
            Err(TypeError::OverlyGeneralExpr) => predicates.extend(
                pred.expr
                    .deepen(a)
                    .iter()
                    .cloned()
                    .map(|expr| TypingPredicate { expr, ..pred }),
            ),
            Err(TypeError::OverlyGeneralType) => predicates.extend(
                pred.expr
                    .deepen_ty(a)
                    .iter()
                    .cloned()
                    .map(|expr| TypingPredicate { expr, ..pred }),
            ),
            Err(_) => {}
        }
    }

    // We generate the deepenings in the order we'd like to see them, so we reverse to restore that
    // order.
    rules.reverse();
    rules
}

pub fn display_rules(options: RuleOptions) {
    println!("The current options can be fully described as the following set of rules.");

    let arenas = &Arenas::default();
    let ctx = TypingCtx { arenas, options };
    if options.downgrade_shared_inside_shared {
        println!(
            "Warning: option `downgrade_shared_inside_shared` is not represented in the rules"
        );
    }

    println!();

    let mut typing_rules = compute_rules(ctx);
    typing_rules.sort_by_key(|rule| rule.name);
    for rule in typing_rules {
        println!("{rule}\n");
    }
}

struct TypingRule<'a> {
    name: Rule,
    preconditions: Vec<TypingPredicate<'a>>,
    postcondition: TypingPredicate<'a>,
}

fn requires_by_move(e: &Expression<'_>) -> bool {
    match e.kind {
        ExprKind::Scrutinee => false,
        ExprKind::Abstract { bm_is_move } => bm_is_move,
        ExprKind::Ref(_, e)
        | ExprKind::Deref(e)
        | ExprKind::Field(e, _)
        | ExprKind::CastAsImmRef(e) => requires_by_move(e),
    }
}

impl<'a> Display for TypingRule<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            name,
            preconditions,
            postcondition,
        } = self;
        let preconditions_str = if preconditions.is_empty() {
            postcondition.display_as_let()
        } else {
            preconditions.iter().format(", ").to_string()
        };
        let mut postcondition_str = postcondition.to_string();
        if requires_by_move(&postcondition.expr) {
            let _ = write!(
                &mut postcondition_str,
                ", binding_mode({}) = move",
                ExprKind::Abstract { bm_is_move: true }
            );
        }

        let len = max(preconditions_str.len(), postcondition_str.len());
        let bar = "-".repeat(len);
        write!(f, "{preconditions_str}\n")?;
        write!(f, "{bar} \"{name:?}\"\n")?;
        write!(f, "{postcondition_str}")?;
        Ok(())
    }
}
