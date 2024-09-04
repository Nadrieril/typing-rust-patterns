use serde::{Deserialize, Serialize};
use std::cmp::max;
use std::collections::HashSet;
use std::fmt::Write;

use itertools::{EitherOrBoth, Itertools};

use crate::*;
use BindingMode::*;

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct TypingRule<'a> {
    pub name: Rule,
    pub preconditions: Vec<TypingPredicate<'a>>,
    pub postcondition: TypingPredicate<'a>,
}

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

    /// A predicate with abstract patterns, type and expression.
    fn new_abstract(a: &'a Arenas<'a>) -> Self {
        TypingPredicate {
            pat: &Pattern::Abstract("p"),
            expr: Expression {
                kind: ExprKind::new_abstract(),
                ty: Type::Abstract("T").alloc(a),
            },
        }
    }
}

const TRACE: bool = false;

/// Compute all the rules that describe the behavior of the solver with the given options. We start
/// with a dummy predicate with abstract pattern, expression and type, and recursively refine it as
/// long as we get `OverlyGeneral` errors. This ensures we explore all possible cases.
pub fn compute_rules<'a>(ctx: TypingCtx<'a>) -> Vec<TypingRule<'a>> {
    let a = ctx.arenas;
    let mut predicates = vec![TypingPredicate::new_abstract(a)];

    let mut rules = Vec::new();
    while let Some(pred) = predicates.pop() {
        if TRACE {
            println!("Analyzing pred: {pred}");
        }
        match pred.typing_rule(ctx) {
            Ok(rule) => {
                if TRACE {
                    let rule_str = rule.display(TypingRuleStyle::Expression).unwrap();
                    let rule_str = rule_str.replace("\n", "\n    ");
                    println!("  Pushing rule:\n    {rule_str}");
                }
                rules.push(rule);
            }
            Err(TypeError::OverlyGeneral(req)) => {
                let new_preds = pred.deepen(a, req, true);
                if TRACE {
                    print!(
                        "  Pushing new preds:\n{}",
                        new_preds.iter().map(|p| format!("    {p}\n")).format("")
                    );
                }
                predicates.extend(new_preds);
            }
            Err(err) => {
                if TRACE {
                    println!("  Type error: {err:?}");
                }
            }
        }
    }

    // We generate the deepenings in the order we'd like to see them, so we reverse to restore that
    // order.
    rules.reverse();
    rules.sort_by_key(|rule| rule.name);
    rules
}

/// Compute rules for two sets of options simultaneously, such that they both have the same set of
/// starting predicates.
pub fn compute_joint_rules<'a>(
    a: &'a Arenas<'a>,
    left: RuleOptions,
    right: RuleOptions,
) -> Vec<EitherOrBoth<TypingRule<'a>>> {
    use EitherOrBoth::*;
    let mut predicates = vec![TypingPredicate::new_abstract(a)];
    let left = TypingCtx {
        arenas: a,
        options: left,
    };
    let right = TypingCtx {
        arenas: a,
        options: right,
    };

    let mut rules = Vec::new();
    while let Some(pred) = predicates.pop() {
        match (pred.typing_rule(left), pred.typing_rule(right)) {
            (Ok(left), Ok(right)) => rules.push(Both(left, right)),
            (Err(TypeError::OverlyGeneral(req)), _) | (_, Err(TypeError::OverlyGeneral(req))) => {
                predicates.extend(pred.deepen(a, req, true))
            }
            (Ok(left), Err(_)) => rules.push(Left(left)),
            (Err(_), Ok(right)) => rules.push(Right(right)),
            (Err(_), Err(_)) => {}
        }
    }

    // We generate the deepenings in the order we'd like to see them, so we reverse to restore that
    // order.
    rules.reverse();
    rules.sort_by_key(|joint_rule| match joint_rule {
        Both(x, _) | Left(x) | Right(x) => x.name,
    });
    rules
}

/// Extra constraints to display as preconditions.
#[derive(Default)]
pub struct SideConstraints<'a> {
    /// The binding mode of the abstract expression.
    pub binding_mode: Option<BindingMode>,
    /// Whether the abstract expression is known not to be a reference.
    pub abstract_expr_is_not_ref: bool,
    /// Type variables that are known not to be references.
    pub non_ref_types: HashSet<&'a str>,
    /// What access the abstract expression has of the scrutinee.
    pub scrutinee_mutability: Option<Mutability>,
}

impl<'a> Type<'a> {
    /// Collect the names of types known to not be references.
    fn collect_side_constraints(&self, cstrs: &mut SideConstraints<'a>) {
        self.visit(&mut |ty| match ty {
            Type::NonRef(var) => {
                cstrs.non_ref_types.insert(var);
            }
            _ => {}
        })
    }
}

impl<'a> Expression<'a> {
    /// Collects constraints on the abstract variables of this expression.
    fn collect_side_constraints(&self, cstrs: &mut SideConstraints<'a>) {
        self.ty.collect_side_constraints(cstrs);
        self.visit(&mut |e| match e.kind {
            ExprKind::Abstract {
                not_a_ref,
                scrutinee_mutability,
            } => {
                cstrs.scrutinee_mutability = scrutinee_mutability;
                cstrs.abstract_expr_is_not_ref = not_a_ref;
            }
            _ => {}
        })
    }

    /// Interprets the expression as a binding mode, or returns `None` if that doesn't make sense.
    fn as_binding_mode(&self) -> Result<Option<BindingMode>, IncompatibleStyle> {
        match self.kind {
            ExprKind::Abstract {
                not_a_ref: false, ..
            } => Ok(None),
            ExprKind::Abstract {
                not_a_ref: true, ..
            } => Ok(Some(ByMove)),
            ExprKind::Ref(
                mtbl,
                Expression {
                    kind:
                        ExprKind::Abstract {
                            not_a_ref: false, ..
                        },
                    ..
                },
            ) => Ok(Some(ByRef(mtbl))),
            _ => Err(IncompatibleStyle),
        }
    }

    /// Changes the abstract variable to have the provided bm. Assumes that the expression was
    /// obtained from applying rules to an expression hwere the abstract variable already had that
    /// binding mode.
    fn set_abstract_bm(&self, a: &'a Arenas<'a>, bm: Option<BindingMode>) -> Self {
        match (self.kind, bm) {
            (ExprKind::Scrutinee, _) => *self,
            (
                ExprKind::Abstract {
                    not_a_ref: false, ..
                },
                None,
            ) => *self,
            (
                ExprKind::Abstract {
                    not_a_ref: false, ..
                },
                Some(ByMove),
            ) => unreachable!(),
            (
                ExprKind::Abstract {
                    not_a_ref: false, ..
                },
                Some(ByRef(mtbl)),
            ) => Expression {
                ty: Type::Ref(mtbl, self.ty).alloc(a),
                kind: self.kind,
            }
            .deref(a),
            (
                ExprKind::Abstract {
                    not_a_ref: true,
                    scrutinee_mutability,
                },
                Some(ByMove),
            ) => Expression {
                ty: self.ty,
                kind: ExprKind::Abstract {
                    not_a_ref: false,
                    scrutinee_mutability,
                },
            },
            (
                ExprKind::Abstract {
                    not_a_ref: true, ..
                },
                _,
            ) => unreachable!(),
            (
                ExprKind::Ref(
                    mtbl,
                    &Expression {
                        kind:
                            ExprKind::Abstract {
                                not_a_ref: false,
                                scrutinee_mutability,
                            },
                        ..
                    },
                ),
                Some(ByRef(bm_mtbl)),
            ) if mtbl == bm_mtbl => Expression {
                ty: self.ty,
                kind: ExprKind::Abstract {
                    not_a_ref: false,
                    scrutinee_mutability,
                },
            },
            (ExprKind::Ref(mtbl, e), _) => e.set_abstract_bm(a, bm).borrow(a, mtbl),
            (ExprKind::Deref(e), _) => e.set_abstract_bm(a, bm).deref(a),
            (ExprKind::Field(e, n), _) => e.set_abstract_bm(a, bm).field(a, n),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum TypingRuleStyle {
    /// Draws the expression as-is.
    Expression,
    /// Replaces the expression with a binding-mode side-constraint.
    BindingMode,
    /// Doesn't draw the expression.
    Stateless,
}

#[derive(Debug)]
pub struct IncompatibleStyle;

impl<'a> TypingRule<'a> {
    /// Collects the side constraints stored in the expression and type.
    fn collect_side_constraints(&self) -> SideConstraints {
        let mut cstrs = SideConstraints::default();
        self.postcondition.expr.collect_side_constraints(&mut cstrs);
        cstrs
    }

    pub fn display(&self, style: TypingRuleStyle) -> Result<String, IncompatibleStyle> {
        use TypingRuleStyle::*;
        let abstract_expr = ExprKind::new_abstract();
        let a = &Arenas::default();

        let mut cstrs = self.collect_side_constraints();
        if matches!(style, BindingMode | Stateless) {
            // Interpret the expression as a binding mode if possible.
            cstrs.binding_mode = self.postcondition.expr.as_binding_mode()?;
        }

        let mut rule = self.clone();
        match style {
            BindingMode => {
                // Reset the expression to be the abstract one.
                rule.postcondition.expr.kind = abstract_expr;
                // We may have changed the type of `e` so we must change it in the postconditions too.
                for pred in &mut rule.preconditions {
                    pred.expr = pred.expr.set_abstract_bm(a, cstrs.binding_mode);
                }
            }
            Stateless if cstrs.binding_mode.is_some() => {
                return Err(IncompatibleStyle);
            }
            _ => {}
        }

        let mut postconditions_str = rule.postcondition.display_with_style(style);

        match style {
            Expression => {
                if cstrs.abstract_expr_is_not_ref {
                    let _ = write!(
                        &mut postconditions_str,
                        ", {} is not a reference",
                        abstract_expr
                    );
                }
            }
            BindingMode => {
                if let Some(bm) = cstrs.binding_mode {
                    let bm = bm.name();
                    let _ = write!(
                        &mut postconditions_str,
                        ", binding_mode({}) = {bm}",
                        abstract_expr
                    );
                }
            }
            _ => {}
        }
        for ty in cstrs.non_ref_types {
            let _ = write!(&mut postconditions_str, ", {ty} is not a reference",);
        }
        if let Some(mtbl) = cstrs.scrutinee_mutability {
            match style {
                Expression | BindingMode => {
                    let mtbl = match mtbl {
                        Mutability::Shared => "read-only",
                        Mutability::Mutable => "mutable",
                    };
                    let _ = write!(&mut postconditions_str, ", {abstract_expr} {mtbl}",);
                }
                Stateless => return Err(IncompatibleStyle),
            }
        }

        let mut preconditions_str = rule
            .preconditions
            .iter()
            .map(|pred| pred.display_with_style(style))
            .join(",  ");

        if let BindingMode = style
            && let Some(ByRef(..)) = cstrs.binding_mode
        {
            // In binding mode style, dereferencing the bm is called "resetting".
            preconditions_str = preconditions_str.replace("*q", "reset(q)");
        }

        let len = max(preconditions_str.len(), postconditions_str.len());
        let bar = "-".repeat(len);
        let mut out = String::new();
        let _ = write!(&mut out, "{preconditions_str}\n");
        let _ = write!(&mut out, "{bar} \"{:?}\"\n", self.name);
        let _ = write!(&mut out, "{postconditions_str}");
        Ok(out)
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
        .into_iter()
        .copied()
        .cartesian_product([
            TypingRuleStyle::Expression,
            TypingRuleStyle::BindingMode,
            TypingRuleStyle::Stateless,
        ])
        .map(|((name, options, _), style)| (name, options, style));

    for (name, options, style) in bundles {
        let ctx = TypingCtx { arenas, options };

        let mut typing_rules = compute_rules(ctx);
        typing_rules.sort_by_key(|rule| rule.name);

        let mut rules_str = String::new();
        let _: Result<_, IncompatibleStyle> = try {
            for rule in typing_rules {
                let _ = writeln!(&mut rules_str, "{}\n", rule.display(style)?);
            }

            let info = TestCase {
                bundle_name: name,
                options,
            };
            insta::with_settings!({
                snapshot_path => "../tests/snapshots",
                snapshot_suffix => format!("{name}-{:?}", style),
                prepend_module_to_snapshot => false,
                omit_expression => true,
                info => &info,
            }, {
                insta::assert_snapshot!(rules_str);
            });
        };
    }

    Ok(())
}
