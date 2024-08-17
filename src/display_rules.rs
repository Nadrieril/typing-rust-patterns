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
        bm: BindingMode,
    ) -> Result<Self, ErrorTree<String>> {
        let req = TypingRequest::parse(a, req)?;
        let expr = if let ByRef(mtbl) = bm {
            // Instead of starting with the scrutinee `p` expression, we start with `&p` or `&mut
            // p`. This only makes sense if the type was already the corresponding reference.
            Expression {
                kind: ExprKind::Scrutinee,
                ty: req.ty.deref(),
            }
            .borrow(a, mtbl, false)
        } else {
            Expression {
                kind: ExprKind::Scrutinee,
                ty: req.ty,
            }
        };
        Ok(TypingPredicate { pat: req.pat, expr })
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
        .map(|&(case, bm)| {
            let starting_pred = TypingPredicate::parse_request_with_bm(
                arenas,
                case,
                bm.unwrap_or(BindingMode::ByMove),
            )
            .unwrap();
            (starting_pred, bm.is_some())
        })
        .collect_vec();

    if options.simplify_expressions {
        // Add the special expression simplification rules.
        let req = TypingRequest::parse(arenas, "p: T").unwrap();
        let mut_borrow = &Expression {
            kind: ExprKind::Scrutinee,
            ty: req.ty,
        }
        .borrow(arenas, Mutable, false);
        starting_predicates.push((
            TypingPredicate {
                pat: req.pat,
                expr: mut_borrow.cast_as_imm_ref(arenas),
            },
            false,
        ));
        starting_predicates.push((
            TypingPredicate {
                pat: req.pat,
                expr: mut_borrow.deref(arenas),
            },
            false,
        ));
    }

    let mut typing_rules = starting_predicates
        .into_iter()
        .filter_map(|(starting_pred, enforce_bm)| {
            let (rule, preconditions) = starting_pred.step(ctx).ok()?;
            let tyrule = TypingRule {
                name: rule,
                preconditions,
                postcondition: starting_pred,
                require_by_move: enforce_bm && starting_pred.expr.binding_mode() == ByMove,
            };
            Some(tyrule)
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
    /// Adds a `binding_mode(<expr>) = move` condition
    require_by_move: bool,
}

impl<'a> Display for TypingRule<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let Self {
            name,
            preconditions,
            postcondition,
            require_by_move,
        } = self;
        let preconditions_str = if preconditions.is_empty() {
            postcondition.display_as_let()
        } else {
            preconditions.iter().format(", ").to_string()
        };
        let mut postcondition_str = postcondition.to_string();
        if *require_by_move {
            let _ = write!(
                &mut postcondition_str,
                ", binding_mode({}) = move",
                postcondition.expr
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
