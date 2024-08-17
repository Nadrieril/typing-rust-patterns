use std::fmt::Write;
use std::{cmp::max, fmt::Display};

use itertools::Itertools;
use nom_supreme::error::ErrorTree;

use crate::*;
use BindingMode::*;
use Mutability::*;

impl<'a> TypingPredicate<'a> {
    /// Starting point for most rules: a request and a binding mode. Panics if the binding mode is
    /// not compatible with the type in the request.
    fn parse_request_with_bm(
        a: &'a Arenas<'a>,
        req: &str,
        bm: Option<BindingMode>,
    ) -> Result<Self, ErrorTree<String>> {
        let req = TypingRequest::parse(a, req)?;
        let kind = ExprKind::Abstract {
            bm_is_move: bm == Some(ByMove),
        };
        let expr = if let Some(ByRef(mtbl)) = bm {
            // Instead of starting with the scrutinee `p` expression, we start with `&p` or `&mut
            // p`. This only makes sense if the type was already the corresponding reference.
            Expression {
                kind,
                ty: req.ty.deref(),
            }
            .borrow(a, mtbl, false)
        } else {
            Expression { kind, ty: req.ty }
        };
        Ok(TypingPredicate { pat: req.pat, expr })
    }

    fn typing_rule(&self, ctx: TypingCtx<'a>) -> Result<TypingRule<'a>, TypeError> {
        let (rule, preconditions) = self.step(ctx)?;
        Ok(TypingRule {
            name: rule,
            preconditions,
            postcondition: *self,
        })
    }
}
}

pub fn display_rules(options: RuleOptions) {
    let arenas = &Arenas::default();
    let ctx = TypingCtx { arenas, options };
    if options.downgrade_shared_inside_shared {
        println!(
            "Warning: option `downgrade_shared_inside_shared` is not represented in the rules"
        );
    }

    // We know how deep each rule inspects the predicate. We can therefore make a list of
    // predicates that exercises all rules and codepaths.
    // The binding mode is `None` if it never matters for the given case.
    let basic_cases = [
        // Constructor rules
        ("[p0, p1]: [T0, T1]", None),
        ("[p0, p1]: &[T0, T1]", None),
        ("[p0, p1]: &mut [T0, T1]", None),
        ("[p0, p1]: &&T", None),
        ("[p0, p1]: &mut &T", None),
        ("[p0, p1]: &&mut T", None),
        ("[p0, p1]: &mut &mut T", None),
        // Dereference rules
        ("&p: &T", Some(ByMove)),
        ("&p: &mut T", Some(ByMove)),
        ("&p: &&T", Some(ByRef(Shared))),
        ("&p: &mut &T", Some(ByRef(Mutable))),
        ("&p: &&mut T", Some(ByRef(Shared))),
        ("&p: &mut &mut T", Some(ByRef(Mutable))),
        ("&mut p: &T", Some(ByMove)),
        ("&mut p: &mut T", Some(ByMove)),
        ("&mut p: &&T", Some(ByRef(Shared))),
        ("&mut p: &mut &T", Some(ByRef(Mutable))),
        ("&mut p: &&mut T", Some(ByRef(Shared))),
        ("&mut p: &mut &mut T", Some(ByRef(Mutable))),
        // Binding rules
        ("ref x: T", Some(ByMove)),
        ("ref x: &T", Some(ByRef(Shared))),
        ("ref x: &mut T", Some(ByRef(Mutable))),
        ("ref mut x: T", Some(ByMove)),
        ("ref mut x: &T", Some(ByRef(Shared))),
        ("ref mut x: &mut T", Some(ByRef(Mutable))),
        ("x: T", Some(ByMove)),
        ("x: &T", Some(ByRef(Mutable))),
        ("x: &mut T", Some(ByRef(Shared))),
        ("mut x: T", Some(ByMove)),
        ("mut x: &T", Some(ByRef(Shared))),
        ("mut x: &mut T", Some(ByRef(Mutable))),
    ];
    let mut starting_predicates = basic_cases
        .iter()
        .map(|&(case, bm)| TypingPredicate::parse_request_with_bm(arenas, case, bm).unwrap())
        .collect_vec();

    if options.simplify_expressions {
        // Add the special expression simplification rules.
        let req = TypingRequest::parse(arenas, "p: T").unwrap();
        let mut_borrow = &Expression {
            kind: ExprKind::Scrutinee,
            ty: req.ty,
        }
        .borrow(arenas, Mutable, false);
        starting_predicates.push(TypingPredicate {
            pat: req.pat,
            expr: mut_borrow.cast_as_imm_ref(arenas),
        });
        starting_predicates.push(TypingPredicate {
            pat: req.pat,
            expr: mut_borrow.deref(arenas),
        });
    }

    let mut typing_rules = starting_predicates
        .into_iter()
        .filter_map(|starting_pred| match starting_pred.typing_rule(ctx) {
            Ok(x) => Some(x),
            Err(
                err @ (TypeError::OverlyGeneralPattern
                | TypeError::OverlyGeneralExpr
                | TypeError::OverlyGeneralType),
            ) => panic!("overly general predicate: `{starting_pred}` ({err:?})"),
            Err(_) => None,
        })
        .collect_vec();
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
