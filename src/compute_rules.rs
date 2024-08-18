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
    fn deepen(&'a self, a: &'a Arenas<'a>) -> Vec<Self> {
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
                    )
                    .collect()
            }
            Pattern::Tuple(pats) => {
                // Collect to avoid arena reentrancy problems.
                pats.iter()
                    .map(|p| p.deepen(a))
                    .multi_cartesian_product()
                    .map(|pats| Pattern::Tuple(a.pat_arena.alloc_extend(pats)))
                    .collect()
            }
            Pattern::Ref(mtbl, p) => p
                .deepen(a)
                .into_iter()
                .map(|p| Pattern::Ref(mtbl, p.alloc(a)))
                .collect(),
            Pattern::Binding(_, _, _) => vec![*self],
        }
    }
}

impl<'a> Type<'a> {
    /// Replace abstract subtypes with all the possible more-precise types.
    fn deepen(&'a self, a: &'a Arenas<'a>) -> Vec<Self> {
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
                vec![tuple, Type::Ref(Shared, self), Type::Ref(Mutable, self)]
            }
            Type::Tuple(tys) => tys
                .iter()
                .map(|p| p.deepen(a))
                .multi_cartesian_product()
                .map(|tys| Type::Tuple(a.type_arena.alloc_extend(tys)))
                .collect(),
            Type::Ref(mtbl, p) => p
                .deepen(a)
                .into_iter()
                .map(|p| Type::Ref(mtbl, p.alloc(a)))
                .collect(),
        }
    }
}

impl<'a> Expression<'a> {
    /// Replace abstract subexpressions with all the possible more-precise expressions.
    fn deepen(&self, a: &'a Arenas<'a>) -> Vec<Self> {
        match self.kind {
            ExprKind::Scrutinee => vec![*self],
            ExprKind::Abstract { not_a_ref: false } => {
                // We know our rules only inspect the binding modes of expressions, so we only need
                // to split along that dimension.
                let mut vec = vec![Expression {
                    // Stands for any non-`Ref` expression.
                    kind: ExprKind::Abstract { not_a_ref: true },
                    ty: self.ty,
                }];
                // Add more `Ref` expressions, following the type if relevant.
                match *self.ty {
                    Type::Ref(mtbl, ty) => {
                        vec.push(
                            Expression {
                                kind: self.kind,
                                ty,
                            }
                            .borrow(a, mtbl),
                        );
                    }
                    Type::Var(..) => {
                        vec.push(self.borrow(a, Shared));
                        vec.push(self.borrow(a, Mutable));
                    }
                    Type::Tuple(..) => {}
                }
                vec
            }
            // We never generate these.
            ExprKind::Deref(_) | ExprKind::Field(_, _) => {
                unreachable!()
            }
            ExprKind::Ref(..) | ExprKind::Abstract { not_a_ref: true } => {
                unreachable!("A rule is inspecting expressions in unexpected ways")
            }
        }
    }

    /// Replace abstract subtypes with all the possible more-precise types.
    fn deepen_ty(&self, a: &'a Arenas<'a>) -> Vec<Self> {
        match self.kind {
            ExprKind::Scrutinee | ExprKind::Abstract { .. } => self
                .ty
                .deepen(a)
                .into_iter()
                .map(|ty| Expression {
                    kind: self.kind,
                    ty: ty.alloc(a),
                })
                .collect(),
            ExprKind::Ref(mtbl, e) => e
                .deepen_ty(a)
                .into_iter()
                .map(|e| e.borrow(a, mtbl))
                .collect(),
            // We never generate these.
            ExprKind::Deref(_) | ExprKind::Field(_, _) => {
                unreachable!()
            }
        }
    }
}

const TRACE: bool = false;

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
pub fn compute_rules<'a>(ctx: TypingCtx<'a>) -> Vec<TypingRule<'a>> {
    let a = ctx.arenas;
    let mut predicates = vec![TypingPredicate {
        pat: &Pattern::Abstract("p"),
        expr: Expression {
            kind: ExprKind::Abstract { not_a_ref: false },
            ty: Type::Var("T").alloc(a),
        },
    }];

    // Add the special expression simplification predicate because we won't explore it since it
    // doesn't trigger `OverlyGeneralExpr` errors.
    if ctx.options.simplify_expressions {
        let e = &Expression {
            kind: ExprKind::Scrutinee,
            ty: &Type::Var("T"),
        };
        predicates.push(TypingPredicate {
            pat: &Pattern::Abstract("p"),
            expr: e.borrow(a, Mutable).deref(a),
        });
    }

    let mut rules = Vec::new();
    while let Some(pred) = predicates.pop() {
        if TRACE {
            println!("Analyzing pred: {pred}");
        }
        let new_preds = match pred.typing_rule(ctx) {
            Ok(rule) => {
                if TRACE {
                    let rule_str = rule.to_string();
                    let rule_str = rule_str.replace("\n", "\n    ");
                    println!("  Pushing rule:\n    {rule_str}");
                }
                rules.push(rule);
                vec![]
            }
            Err(TypeError::OverlyGeneralPattern) => pred
                .pat
                .deepen(a)
                .into_iter()
                .map(|pat| pat.alloc(a))
                .map(|pat| TypingPredicate { pat, ..pred })
                .collect_vec(),
            Err(TypeError::OverlyGeneralExpr) => pred
                .expr
                .deepen(a)
                .into_iter()
                .map(|expr| TypingPredicate { expr, ..pred })
                .collect_vec(),
            Err(TypeError::OverlyGeneralType) => pred
                .expr
                .deepen_ty(a)
                .into_iter()
                .map(|expr| TypingPredicate { expr, ..pred })
                .collect_vec(),
            Err(err) => {
                if TRACE {
                    println!("  Type error: {err:?}");
                }
                vec![]
            }
        };
        if TRACE && !new_preds.is_empty() {
            print!(
                "  Pushing new preds:\n{}",
                new_preds.iter().map(|p| format!("    {p}\n")).format("")
            );
        }
        predicates.extend(new_preds);
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

#[derive(Clone)]
pub struct TypingRule<'a> {
    pub name: Rule,
    pub preconditions: Vec<TypingPredicate<'a>>,
    pub postcondition: TypingPredicate<'a>,
}

impl<'a> Expression<'a> {
    /// Whether the abstract variable requires a `binding_mode` constaint.
    fn abstract_bm_constraint(self) -> Option<BindingMode> {
        match self.kind {
            ExprKind::Scrutinee => None,
            ExprKind::Abstract { not_a_ref } => not_a_ref.then_some(ByMove),
            ExprKind::Ref(_, e) | ExprKind::Deref(e) | ExprKind::Field(e, _) => {
                e.abstract_bm_constraint()
            }
        }
    }

    /// If the tail of this expression is `Abstract`, removes the binding mode on that variable and
    /// returns it. Beware: this changes the type of the variable. We must apply the same
    /// transformation to the preconditions.
    fn extract_abstract_bm(&self, a: &'a Arenas<'a>) -> (Option<BindingMode>, Expression<'a>) {
        match self.kind {
            ExprKind::Scrutinee => (None, *self),
            ExprKind::Abstract { not_a_ref: false } => (None, *self),
            ExprKind::Abstract { not_a_ref: true } => (
                Some(ByMove),
                Expression {
                    ty: self.ty,
                    kind: ExprKind::Abstract { not_a_ref: false },
                },
            ),
            ExprKind::Ref(
                mtbl,
                Expression {
                    kind: ExprKind::Abstract { not_a_ref: false },
                    ..
                },
            ) => (
                Some(ByRef(mtbl)),
                Expression {
                    ty: self.ty,
                    kind: ExprKind::Abstract { not_a_ref: false },
                },
            ),
            ExprKind::Ref(mtbl, e) => {
                let (bm, e) = e.extract_abstract_bm(a);
                (bm, e.borrow(a, mtbl))
            }
            ExprKind::Deref(e) => {
                let (bm, e) = e.extract_abstract_bm(a);
                (bm, e.deref(a))
            }
            ExprKind::Field(e, n) => {
                let (bm, e) = e.extract_abstract_bm(a);
                (bm, e.field(a, n))
            }
        }
    }

    /// Changes the abstract variable to have the provided bm. Assumes that the expression was
    /// obtained from applying rules to an expression hwere the abstract variable already had that
    /// binding mode.
    fn set_abstract_bm(&self, a: &'a Arenas<'a>, bm: Option<BindingMode>) -> Expression<'a> {
        match (self.kind, bm) {
            (ExprKind::Scrutinee, _) => *self,
            (ExprKind::Abstract { not_a_ref: false }, None) => *self,
            (ExprKind::Abstract { not_a_ref: false }, Some(ByMove)) => unreachable!(),
            (ExprKind::Abstract { not_a_ref: false }, Some(ByRef(mtbl))) => Expression {
                ty: Type::Ref(mtbl, self.ty).alloc(a),
                kind: self.kind,
            }
            .deref(a),
            (ExprKind::Abstract { not_a_ref: true }, Some(ByMove)) => Expression {
                ty: self.ty,
                kind: ExprKind::Abstract { not_a_ref: false },
            },
            (ExprKind::Abstract { not_a_ref: true }, _) => unreachable!(),
            (
                ExprKind::Ref(
                    mtbl,
                    Expression {
                        kind: ExprKind::Abstract { not_a_ref: false },
                        ..
                    },
                ),
                Some(ByRef(bm_mtbl)),
            ) if mtbl == bm_mtbl => Expression {
                ty: self.ty,
                kind: ExprKind::Abstract { not_a_ref: false },
            },
            (ExprKind::Ref(mtbl, e), _) => e.set_abstract_bm(a, bm).borrow(a, mtbl),
            (ExprKind::Deref(e), _) => e.set_abstract_bm(a, bm).deref(a),
            (ExprKind::Field(e, n), _) => e.set_abstract_bm(a, bm).field(a, n),
        }
    }
}

pub enum TypingRuleStyle {
    // TODO: Rename binding mode to e.g. "is not ref"
    PlainPredicate,
    SeparateBindingMode,
    // TODO: implement
    HideExpr,
}

impl<'a> TypingRule<'a> {
    /// If the postcondition expression contains an abstract variable with a known binding mode,
    /// extract it and reset the binding mode of the variable.
    fn extract_abstract_bm(&self, a: &'a Arenas<'a>) -> (Option<BindingMode>, Self) {
        let mut ret = self.clone();
        let (bm, expr) = ret.postcondition.expr.extract_abstract_bm(a);
        ret.postcondition.expr = expr;
        // If we changed the type of `q` above, we must change it here too.
        for pred in &mut ret.preconditions {
            pred.expr = pred.expr.set_abstract_bm(a, bm);
        }
        (bm, ret)
    }

    fn display(&self, f: &mut std::fmt::Formatter<'_>, style: TypingRuleStyle) -> std::fmt::Result {
        let a = &Arenas::default();

        let mut rule = self.clone();

        let bm = match style {
            TypingRuleStyle::PlainPredicate => rule.postcondition.expr.abstract_bm_constraint(),
            TypingRuleStyle::SeparateBindingMode | TypingRuleStyle::HideExpr => {
                // Extract the bm of the expression variable and show it on the side.
                let (bm, new_rule) = rule.extract_abstract_bm(a);
                rule = new_rule;
                bm
            }
        };

        let mut postconditions_str = rule.postcondition.to_string();
        if let Some(bm) = bm {
            let bm = match bm {
                ByRef(mtbl) => &format!("ref {mtbl}"),
                ByMove => "move",
            };
            let bm = bm.trim();
            let _ = write!(
                &mut postconditions_str,
                ", binding_mode({}) = {bm}",
                ExprKind::Abstract { not_a_ref: true }
            );
        }

        let preconditions_str = if rule.preconditions.is_empty() {
            rule.postcondition.display_as_let()
        } else {
            rule.preconditions.iter().format(", ").to_string()
        };

        let len = max(preconditions_str.len(), postconditions_str.len());
        let bar = "-".repeat(len);
        write!(f, "{preconditions_str}\n")?;
        write!(f, "{bar} \"{:?}\"\n", rule.name)?;
        write!(f, "{postconditions_str}")?;
        Ok(())
    }
}

impl<'a> Display for TypingRule<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.display(f, TypingRuleStyle::PlainPredicate)
    }
}
