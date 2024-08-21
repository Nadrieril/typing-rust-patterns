use serde::{Deserialize, Serialize};
use std::fmt::Write;
use std::{cmp::max, fmt::Display};

use itertools::Itertools;

use crate::*;
use BindingMode::*;

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

const TRACE: bool = false;

/// Compute all the rules that describe the behavior of the solver with the given options. We start
/// with a dummy predicate with abstract pattern, expression and type, and recursively refine it as
/// long as we get `OverlyGeneral` errors. This ensures we explore all possible cases.
///
/// Notable exception is the `downgrade_mut_inside_shared`, which is not possible to describe it
/// as a rule without tracking additional state. As such, it won't emit `OverlyGeneral` errors.
pub fn compute_rules<'a>(ctx: TypingCtx<'a>) -> Vec<TypingRule<'a>> {
    let a = ctx.arenas;
    let mut predicates = vec![TypingPredicate {
        pat: &Pattern::Abstract("p"),
        expr: Expression {
            kind: ExprKind::Abstract { not_a_ref: false },
            ty: Type::Abstract("T").alloc(a),
        },
    }];

    let mut rules = Vec::new();
    while let Some(pred) = predicates.pop() {
        if TRACE {
            println!("Analyzing pred: {pred}");
        }
        let new_preds = match pred.typing_rule(ctx) {
            Ok(rule) => {
                if TRACE {
                    let rule_str = rule.display(TypingRuleStyle::Plain).to_string();
                    let rule_str = rule_str.replace("\n", "\n    ");
                    println!("  Pushing rule:\n    {rule_str}");
                }
                rules.push(rule);
                vec![]
            }
            Err(TypeError::OverlyGeneral(req)) => pred.deepen(a, req, true),
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
    rules.sort_by_key(|rule| rule.name);
    rules
}

#[derive(Clone, PartialEq, Eq, Hash)]
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
    fn extract_abstract_bm(&self, a: &'a Arenas<'a>) -> (Option<BindingMode>, Self) {
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
    fn set_abstract_bm(&self, a: &'a Arenas<'a>, bm: Option<BindingMode>) -> Self {
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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TypingRuleStyle {
    Plain,
    BindingMode,
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

    pub fn display(&self, style: TypingRuleStyle) -> impl Display + '_ {
        TypingRuleWithStyle(self, style)
    }

    fn display_inner(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        style: TypingRuleStyle,
    ) -> std::fmt::Result {
        let a = &Arenas::default();

        let mut preconditions_str;
        let mut postconditions_str;

        // TODO: extract the `Type::NonRef` constraints.
        match style {
            TypingRuleStyle::Plain => {
                let bm = self.postcondition.expr.abstract_bm_constraint();

                postconditions_str = self.postcondition.to_string();
                if let Some(bm) = bm {
                    let abstract_expr = ExprKind::Abstract { not_a_ref: true };
                    assert!(bm == ByMove);
                    let _ = write!(
                        &mut postconditions_str,
                        ", {} is not a reference",
                        abstract_expr
                    );
                }

                preconditions_str = if self.preconditions.is_empty() {
                    self.postcondition.display_as_let()
                } else {
                    self.preconditions.iter().format(",  ").to_string()
                };
            }
            TypingRuleStyle::BindingMode => {
                let mut rule = self.clone();
                // Extract the bm of the expression variable and show it on the side.
                let (bm, new_rule) = rule.extract_abstract_bm(a);
                rule = new_rule;

                postconditions_str = rule.postcondition.to_string();
                if let Some(bm) = bm {
                    let abstract_expr = ExprKind::Abstract { not_a_ref: true };
                    let bm = bm.name();
                    let _ = write!(
                        &mut postconditions_str,
                        ", binding_mode({}) = {bm}",
                        abstract_expr
                    );
                }

                preconditions_str = if rule.preconditions.is_empty() {
                    rule.postcondition.display_as_let()
                } else {
                    rule.preconditions
                        .iter()
                        .map(|pred| pred.to_string())
                        .join(",  ")
                };
                if let Some(ByRef(..)) = bm {
                    // In binding mode style, dereferencing the bm is called "resetting".
                    preconditions_str = preconditions_str.replace("*q", "reset(q)");
                }
            }
        }

        let len = max(preconditions_str.len(), postconditions_str.len());
        let bar = "-".repeat(len);
        write!(f, "{preconditions_str}\n")?;
        write!(f, "{bar} \"{:?}\"\n", self.name)?;
        write!(f, "{postconditions_str}")?;
        Ok(())
    }
}

struct TypingRuleWithStyle<'a>(&'a TypingRule<'a>, TypingRuleStyle);
impl<'a> Display for TypingRuleWithStyle<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.display_inner(f, self.1)
    }
}

#[test]
/// Compute the rulesets for each known bundle.
fn bundle_rules() -> anyhow::Result<()> {
    #[derive(Serialize)]
    struct TestCase {
        bundle_name: &'static str,
        options: RuleOptions,
    }

    let arenas = &Arenas::default();

    // Try both styles
    let bundles = RuleOptions::KNOWN_OPTION_BUNDLES
        .iter()
        .cartesian_product([TypingRuleStyle::Plain, TypingRuleStyle::BindingMode])
        .map(|(&(name, options, _), style)| {
            let options = RuleOptions {
                rules_display_style: style,
                ..options
            };
            (name, options)
        });

    for (name, options) in bundles {
        let ctx = TypingCtx { arenas, options };

        let mut typing_rules = compute_rules(ctx);
        typing_rules.sort_by_key(|rule| rule.name);

        let mut rules_str = String::new();
        for rule in typing_rules {
            let _ = writeln!(
                &mut rules_str,
                "{}\n",
                rule.display(options.rules_display_style)
            );
        }

        let info = TestCase {
            bundle_name: name,
            options,
        };
        insta::with_settings!({
            snapshot_path => "../tests/snapshots",
            snapshot_suffix => format!("{name}-{:?}", options.rules_display_style),
            prepend_module_to_snapshot => false,
            omit_expression => true,
            info => &info,
        }, {
            insta::assert_snapshot!(rules_str);
        });
    }

    Ok(())
}
