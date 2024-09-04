use serde::{Deserialize, Serialize};
use std::collections::HashSet;
use std::fmt::Write;
use std::{cmp::max, fmt::Display};

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
                    let rule_str = rule.display(TypingRuleStyle::Expression).to_string();
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
    /// Type variables that are known not to be references.
    pub non_ref_types: HashSet<&'a str>,
    /// WHat access the abstract expression has of the scrutinee.
    pub scrutinee_mutability: Option<Mutability>,
}

impl<'a> Type<'a> {
    /// Collect the names of types known to not be references.
    fn extract_side_constraints(&self, cstrs: &mut SideConstraints<'a>) {
        match *self {
            Type::Abstract(_) => {}
            Type::NonRef(var) => {
                cstrs.non_ref_types.insert(var);
            }
            Type::Tuple(tys) => {
                for ty in tys {
                    ty.extract_side_constraints(cstrs)
                }
            }
            Type::Ref(_, ty) => ty.extract_side_constraints(cstrs),
        }
    }
}

impl<'a> Expression<'a> {
    /// Collects constraints on the abstract variables of this expression. If `remove_bm` is true,
    /// this counts the whole binding mode of the abstract var as a constraint and removes it from
    /// the expression.
    fn extract_side_constraints(
        &self,
        a: &'a Arenas<'a>,
        cstrs: &mut SideConstraints<'a>,
        remove_bm: bool,
    ) -> Self {
        self.ty.extract_side_constraints(cstrs);
        match self.kind {
            ExprKind::Scrutinee => *self,
            ExprKind::Abstract {
                not_a_ref,
                scrutinee_mutability,
            } => {
                cstrs.scrutinee_mutability = scrutinee_mutability;
                if not_a_ref {
                    cstrs.binding_mode = Some(ByMove);
                    Expression {
                        ty: self.ty,
                        kind: ExprKind::Abstract {
                            not_a_ref: false,
                            scrutinee_mutability,
                        },
                    }
                } else {
                    *self
                }
            }
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
            ) if remove_bm => {
                cstrs.binding_mode = Some(ByRef(mtbl));
                cstrs.scrutinee_mutability = scrutinee_mutability;
                Expression {
                    ty: self.ty,
                    kind: ExprKind::Abstract {
                        not_a_ref: false,
                        scrutinee_mutability,
                    },
                }
            }
            ExprKind::Ref(mtbl, e) => e
                .extract_side_constraints(a, cstrs, remove_bm)
                .borrow(a, mtbl),
            ExprKind::Deref(e) => e.extract_side_constraints(a, cstrs, remove_bm).deref(a),
            ExprKind::Field(e, n) => e.extract_side_constraints(a, cstrs, remove_bm).field(a, n),
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
    Expression,
    /// Replaces the innermost `&{mut}e` expression with a `bm(e) = ref {mut}` side-constraint.
    BindingMode,
    /// Doesn't draw the expression.
    Stateless,
}

impl<'a> TypingRule<'a> {
    /// If the postcondition expression contains an abstract variable with a known binding mode,
    /// extract it and reset the binding mode of the variable.
    fn extract_side_constraints(
        &self,
        a: &'a Arenas<'a>,
        remove_bm: bool,
    ) -> (SideConstraints, Self) {
        let mut cstrs = SideConstraints::default();
        let mut ret = self.clone();
        let expr = ret
            .postcondition
            .expr
            .extract_side_constraints(a, &mut cstrs, remove_bm);
        ret.postcondition.expr = expr;
        // If we changed the type of `q` above, we must change it here too.
        for pred in &mut ret.preconditions {
            pred.expr = pred.expr.set_abstract_bm(a, cstrs.binding_mode);
        }
        (cstrs, ret)
    }

    pub fn display(&self, style: TypingRuleStyle) -> impl Display + '_ {
        TypingRuleWithStyle(self, style)
    }

    fn display_inner(
        &self,
        f: &mut std::fmt::Formatter<'_>,
        style: TypingRuleStyle,
    ) -> std::fmt::Result {
        use TypingRuleStyle::*;
        let a = &Arenas::default();

        let extract_bm = matches!(style, BindingMode);
        let (cstrs, rule) = self.extract_side_constraints(a, extract_bm);
        let abstract_expr = ExprKind::new_abstract();

        let mut postconditions_str = rule.postcondition.display_with_style(style);

        if let Some(bm) = cstrs.binding_mode {
            match style {
                Expression => {
                    assert!(bm == ByMove);
                    let _ = write!(
                        &mut postconditions_str,
                        ", {} is not a reference",
                        abstract_expr
                    );
                }
                BindingMode => {
                    let bm = bm.name();
                    let _ = write!(
                        &mut postconditions_str,
                        ", binding_mode({}) = {bm}",
                        abstract_expr
                    );
                }
                Stateless => {
                    let _ = write!(
                        &mut postconditions_str,
                        ", error: stateless style cannot describe a binding mode",
                    );
                }
            }
        }

        for ty in cstrs.non_ref_types {
            let _ = write!(&mut postconditions_str, ", {ty} is not a reference",);
        }
        if let Some(mtbl) = cstrs.scrutinee_mutability {
            let mtbl = match mtbl {
                Mutability::Shared => "read-only",
                Mutability::Mutable => "mutable",
            };
            let _ = write!(&mut postconditions_str, ", {abstract_expr} {mtbl}",);
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
        .into_iter()
        .copied()
        .cartesian_product([TypingRuleStyle::Expression, TypingRuleStyle::BindingMode])
        .chain([(
            ("stateless", RuleOptions::STATELESS, ""),
            TypingRuleStyle::Stateless,
        )])
        .map(|((name, options, _), style)| (name, options, style));

    for (name, options, style) in bundles {
        let ctx = TypingCtx { arenas, options };

        let mut typing_rules = compute_rules(ctx);
        typing_rules.sort_by_key(|rule| rule.name);

        let mut rules_str = String::new();
        for rule in typing_rules {
            let _ = writeln!(&mut rules_str, "{}\n", rule.display(style));
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
    }

    Ok(())
}
