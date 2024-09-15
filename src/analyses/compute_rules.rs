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
    /// Remember the options used.
    pub options: RuleOptions,
}

impl<'a> TypingPredicate<'a> {
    /// Compute the typing rule associated with this predicate.
    fn typing_rule(&self, ctx: TypingCtx<'a>) -> Result<TypingRule<'a>, TypeError> {
        let (rule, preconditions) = self.step(ctx)?;
        Ok(TypingRule {
            name: rule,
            preconditions,
            postcondition: *self,
            options: ctx.options,
        })
    }
}

const TRACE: bool = false;

/// Compute all the rules that describe the behavior of the solver with the given options. We start
/// with a dummy predicate with abstract pattern, expression and type, and recursively refine it as
/// long as we get `OverlyGeneral` errors. This ensures we explore all possible cases.
pub fn compute_rules<'a>(ctx: TypingCtx<'a>) -> Vec<TypingRule<'a>> {
    let a = ctx.arenas;
    let mut predicates = vec![TypingPredicate::ABSTRACT];

    let mut rules = Vec::new();
    while let Some(pred) = predicates.pop() {
        if TRACE {
            println!("Analyzing pred: {pred}");
        }
        match pred.typing_rule(ctx) {
            Ok(rule) => {
                if TRACE {
                    let rule_str = rule.display(PredicateStyle::Expression).unwrap();
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
    let mut predicates = vec![TypingPredicate::ABSTRACT];
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
            Type::AbstractNonRef(var) => {
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
#[wasm_bindgen::prelude::wasm_bindgen]
pub enum PredicateStyle {
    /// Draws the expression as-is.
    Expression,
    /// Replaces the expression with a binding-mode side-constraint.
    BindingMode,
    /// Tracks the two bits of state on the lhs of a sequent.
    Sequent,
    /// Like `Sequent` but hides the inherited reference and uses DBM terminology.
    SequentBindingMode,
    /// Doesn't draw the expression.
    Stateless,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PredicateExplanation {
    pub pred: String,
    pub components: Vec<String>,
}

impl PredicateStyle {
    pub fn explain_predicate(self) -> PredicateExplanation {
        let pred = TypingPredicate::ABSTRACT.display(self);

        let mut components = vec![];
        match self {
            PredicateStyle::Expression | PredicateStyle::BindingMode => {
                components.push(format!("{} is an expression", "e".code()));
            }
            PredicateStyle::Sequent => {
                components.push(format!(
                    "{} is {} or {} and indicates whether \
                        the outermost reference type (if any) is inherited or not;",
                    "r".code(),
                    "inh".code(),
                    "real".code()
                ));
            }
            PredicateStyle::SequentBindingMode => {
                components.push(format!(
                    "{} is {}, {} or {} and indicates the binding mode;",
                    "bm".code(),
                    "move".code(),
                    "ref".code(),
                    "ref mut".code(),
                ));
            }
            PredicateStyle::Stateless => {}
        }
        match self {
            PredicateStyle::Sequent | PredicateStyle::SequentBindingMode => {
                components.push(format!(
                    "{} is {} or {} and indicates whether \
                        we have mutable or read-only access to the original scrutinee;",
                    "m".code(),
                    "rw".code(),
                    "ro".code(),
                ));
            }
            PredicateStyle::Expression
            | PredicateStyle::BindingMode
            | PredicateStyle::Stateless => {}
        }
        components.push(format!("{} is a pattern;", "p".code()));
        components.push(format!("{} is a type.", "T".code()));

        PredicateExplanation { pred, components }
    }
}

#[derive(Debug)]
pub struct IncompatibleStyle;

enum RenderablePredicate<'a> {
    Pred(TypingPredicate<'a>),
    ExprNotRef(ExprKind<'a>),
    TyNotRef(Type<'a>),
    BindingMode(ExprKind<'a>, BindingMode),
    Mutability(ExprKind<'a>, Mutability),
}

impl RenderablePredicate<'_> {
    fn display(&self, style: PredicateStyle) -> String {
        match self {
            RenderablePredicate::Pred(p) => p.display(style),
            RenderablePredicate::ExprNotRef(e) => format!("{e} is not a reference"),
            RenderablePredicate::TyNotRef(ty) => format!("{ty} is not a reference"),
            RenderablePredicate::BindingMode(e, bm) => format!("binding_mode({e}) = {}", bm.name()),
            RenderablePredicate::Mutability(e, mtbl) => {
                let mtbl = match mtbl {
                    Mutability::Shared => "read-only",
                    Mutability::Mutable => "mutable",
                };
                format!("{e} {mtbl}")
            }
        }
    }
}

/// Intermediate representation used in the display process.
struct RenderableTypingRule<'a> {
    name: Rule,
    preconditions: Vec<RenderablePredicate<'a>>,
    postconditions: Vec<RenderablePredicate<'a>>,
    /// Remember the options used.
    options: RuleOptions,
}

impl<'a> TypingRule<'a> {
    /// Collects the side constraints stored in the expression and type.
    fn collect_side_constraints(&self) -> SideConstraints {
        let mut cstrs = SideConstraints::default();
        self.postcondition.expr.collect_side_constraints(&mut cstrs);
        cstrs
    }

    // TODO: deprecate `BindingMode` style
    fn make_renderable(
        &'a self,
        a: &'a Arenas<'a>,
        style: PredicateStyle,
    ) -> Result<RenderableTypingRule<'a>, IncompatibleStyle> {
        use PredicateStyle::*;
        let abstract_expr = ExprKind::ABSTRACT;

        let mut cstrs = self.collect_side_constraints();
        if matches!(style, Sequent | BindingMode | Stateless) {
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
            Stateless if cstrs.binding_mode.is_some() => return Err(IncompatibleStyle),
            SequentBindingMode
                if rule.postcondition.expr.binding_mode().is_err()
                    && matches!(rule.postcondition.expr.ty, Type::Ref(..)) =>
            {
                return Err(IncompatibleStyle)
            }
            _ => {}
        }

        let mut postconditions = vec![RenderablePredicate::Pred(rule.postcondition)];
        match style {
            Expression => {
                if cstrs.abstract_expr_is_not_ref {
                    postconditions.push(RenderablePredicate::ExprNotRef(abstract_expr));
                }
            }
            BindingMode => {
                if let Some(bm) = cstrs.binding_mode {
                    postconditions.push(RenderablePredicate::BindingMode(abstract_expr, bm));
                }
            }
            _ => {}
        }
        for ty in cstrs.non_ref_types {
            postconditions.push(RenderablePredicate::TyNotRef(Type::OtherNonRef(ty)));
        }
        if let Some(mtbl) = cstrs.scrutinee_mutability {
            match style {
                Expression | BindingMode => {
                    postconditions.push(RenderablePredicate::Mutability(abstract_expr, mtbl));
                }
                // We already print this information with the predicate.
                Sequent | SequentBindingMode => {}
                Stateless => return Err(IncompatibleStyle),
            }
        }

        let preconditions = rule
            .preconditions
            .into_iter()
            .map(RenderablePredicate::Pred)
            .collect();

        Ok(RenderableTypingRule {
            name: self.name,
            preconditions,
            postconditions,
            options: self.options,
        })
    }

    pub fn display(&self, style: PredicateStyle) -> Result<String, IncompatibleStyle> {
        let a = &Arenas::default();
        self.make_renderable(a, style)?.display(style)
    }
}

impl<'a> RenderableTypingRule<'a> {
    // TODO: deprecate `BindingMode` style
    pub fn display(&self, style: PredicateStyle) -> Result<String, IncompatibleStyle> {
        use PredicateStyle::*;

        let postconditions_str = self
            .postconditions
            .iter()
            .map(|x| x.display(style))
            .join(", ");
        let mut preconditions_str = self
            .preconditions
            .iter()
            .map(|x| x.display(style))
            .join(",  ");

        if let BindingMode = style
            && self
                .postconditions
                .iter()
                .any(|x| matches!(x, RenderablePredicate::BindingMode(_, ByRef(..))))
        {
            // In binding mode style, dereferencing the bm is called "resetting".
            preconditions_str = preconditions_str.replace("*q", "reset(q)");
        }

        let display_len = if cfg!(target_arch = "wasm32") {
            // Compute string length skipping html tags.
            |s: &str| {
                let mut in_tag = false;
                s.chars()
                    .filter(|&c| {
                        if c == '<' {
                            in_tag = true;
                            false
                        } else if c == '>' {
                            in_tag = false;
                            false
                        } else {
                            !in_tag
                        }
                    })
                    .count()
            }
        } else {
            // Compute string length skipping ansi escape codes.
            ansi_width::ansi_width
        };

        let len = max(
            display_len(&preconditions_str),
            display_len(&postconditions_str),
        );
        let bar = "-".repeat(len);
        let mut out = String::new();
        let _ = write!(&mut out, "{preconditions_str}\n");
        let _ = write!(&mut out, "{bar} \"{}\"\n", self.name.display(self.options));
        let _ = write!(&mut out, "{postconditions_str}");
        Ok(out)
    }
}

#[test]
/// Compute the rulesets for each known bundle.
fn bundle_rules() -> anyhow::Result<()> {
    colored::control::set_override(false);

    #[derive(Serialize)]
    struct TestCase {
        bundle_name: &'static str,
        options: RuleOptions,
    }

    let arenas = &Arenas::default();

    // Try all styles
    let bundles = KNOWN_TY_BASED_BUNDLES
        .iter()
        .copied()
        .cartesian_product([
            PredicateStyle::Expression,
            PredicateStyle::Sequent,
            PredicateStyle::SequentBindingMode,
            PredicateStyle::BindingMode,
            PredicateStyle::Stateless,
        ])
        .map(|(b, style)| (b.name, b.ruleset, style));

    for (name, options, style) in bundles {
        let mut ctx = TypingCtx { arenas, options };
        if matches!(style, PredicateStyle::SequentBindingMode) {
            ctx.options.always_inspect_bm = true;
        }

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
                snapshot_path => "../../tests/snapshots",
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
