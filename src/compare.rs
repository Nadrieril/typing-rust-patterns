use std::cmp::Ordering;

use crate::*;
use match_ergonomics_formality::Conf;

#[derive(Debug, Clone, Copy)]
pub enum RuleSet {
    TypeBased(RuleOptions),
    BindingModeBased(Conf),
}

pub type ParseError = String;

use AnalysisResult::*;
#[derive(Debug)]
pub enum AnalysisResult<'a> {
    Success(Type<'a>),
    BorrowError(Type<'a>, String),
    TypeError(String),
}

impl<'a> AnalysisResult<'a> {
    pub fn cmp(&self, other: &Self) -> Option<Ordering> {
        use Ordering::*;
        match (self, other) {
            // These borrow errors only come from this solver, whose borrow checker is more
            // accurate. Hence if both agree on types we ignore the borrowck error.
            (Success(lty), Success(rty))
            | (BorrowError(lty, _), Success(rty))
            | (Success(lty), BorrowError(rty, _))
                if lty == rty =>
            {
                Some(Equal)
            }
            (Success(_), Success(_)) => None,
            (TypeError(_) | BorrowError(..), TypeError(_) | BorrowError(..)) => Some(Equal),
            (TypeError(_) | BorrowError(..), Success(_)) => Some(Less),
            (Success(_), TypeError(_) | BorrowError(..)) => Some(Greater),
        }
    }
}

impl RuleSet {
    pub fn analyze<'a>(
        &self,
        a: &'a Arenas<'a>,
        req: TypingRequest<'a>,
    ) -> anyhow::Result<AnalysisResult<'a>> {
        match *self {
            RuleSet::TypeBased(options) => analyze_with_this_crate(a, options, req),
            RuleSet::BindingModeBased(conf) => analyze_with_formality(a, conf, req),
        }
    }
}

fn analyze_with_this_crate<'a>(
    a: &'a Arenas<'a>,
    options: RuleOptions,
    req: TypingRequest<'a>,
) -> anyhow::Result<AnalysisResult<'a>> {
    let ctx = TypingCtx { arenas: a, options };
    let mut solver = TypingSolver::new(req);
    let e = loop {
        match solver.step(ctx) {
            Ok(_) => {}
            Err(e) => break e,
        }
    };
    Ok(match e {
        CantStep::Done => {
            assert_eq!(solver.done_predicates.len(), 1);
            let pred = solver.done_predicates[0];
            let ty = *pred.expr.ty;
            match pred.expr.simplify(ctx).borrow_check() {
                // This error isn't handled by `match-ergo-formality` so we ignore it.
                Ok(()) | Err(BorrowCheckError::CantCopyNestedRefMut) => Success(ty),
                Err(err) => BorrowError(ty, format!("{err:?}")),
            }
        }
        CantStep::NoApplicableRule(_, err) => TypeError(format!("{err:?}")),
    })
}

fn analyze_with_formality<'a>(
    a: &'a Arenas<'a>,
    conf: Conf,
    req: TypingRequest<'a>,
) -> anyhow::Result<AnalysisResult<'a>> {
    use match_ergonomics_formality::*;
    let line = format!("let {} = {};", req.pat, req.ty);
    let stmt = LetStmt::from_str(&line).map_err(|e| anyhow::anyhow!("{e:?}"))?;
    let r = Reduction::from_stmt(conf, stmt);
    Ok(match r.to_type() {
        Ok((_ident, ty)) => {
            let ty: String = ty.to_string();
            let ty: Type = Type::parse(a, &ty)?;
            Success(ty)
        }
        Err(e) => TypeError(e.to_string()),
    })
}

pub fn compare_rulesets<'a>(
    a: &'a Arenas<'a>,
    test_cases: &[TypingRequest<'a>],
    left_ruleset: RuleSet,
    expected_order: Ordering,
    right_ruleset: RuleSet,
) -> anyhow::Result<String> {
    use anyhow::Context;
    use std::fmt::Write;
    use Ordering::*;
    let mut trace = String::new();
    for test_case in test_cases {
        let test_case_str = test_case.to_string();
        let left_res = &left_ruleset
            .analyze(a, *test_case)
            .context(test_case_str.clone())?;
        let right_res = &right_ruleset
            .analyze(a, *test_case)
            .context(test_case_str.clone())?;
        match (left_res.cmp(right_res), expected_order) {
            (Some(Equal), _) => continue,
            (Some(Less), Less) => continue,
            (Some(Greater), Greater) => continue,
            _ => {}
        }
        let _ = writeln!(&mut trace, "Difference on `{test_case_str}`:");
        let _ = writeln!(&mut trace, "   left returned: {left_res:?}");
        let _ = writeln!(&mut trace, "  right returned: {right_res:?}");
    }
    Ok(trace)
}

#[test]
/// Compare rulesets with the `ergo-formality` reference implementation.
fn compare() -> anyhow::Result<()> {
    use Ordering::*;
    use RuleSet::*;

    let a = &Arenas::default();
    let compare: &[(&str, RuleSet, Ordering, RuleSet)] = &[
        (
            "default",
            TypeBased(RuleOptions {
                // `ergo-formality` doesn't support the `Keep` option.
                mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
                // `ergo-formality` doesn't support any option.
                ref_binding_on_inherited: RefBindingOnInheritedBehavior::ResetBindingMode,
                ..RuleOptions::DEFAULT
            }),
            Equal,
            BindingModeBased({
                let mut c = Conf::default();
                c.rule1 = true;
                c.rule4_early = true;
                c.rule5 = true;
                c
            }),
        ),
        (
            "stable_rust",
            TypeBased(RuleOptions::STABLE_RUST),
            Equal,
            BindingModeBased(Conf::rfc2005()),
        ),
        (
            "structural",
            TypeBased(RuleOptions::STRUCTURAL),
            Equal,
            BindingModeBased(Conf::pre_rfc2005()),
        ),
        (
            "ergo2024",
            TypeBased(RuleOptions::ERGO2024),
            Equal,
            BindingModeBased(Conf::rfc3627_2024()),
        ),
        (
            "rfc3627_2021",
            TypeBased(RuleOptions::RFC3627_2021),
            Equal,
            BindingModeBased(Conf::rfc3627_2021()),
        ),
        (
            "ergo2024_breaking_only",
            TypeBased(RuleOptions::ERGO2024_BREAKING_ONLY),
            Equal,
            BindingModeBased(Conf::rfc_3627_2024_min()),
        ),
        (
            "waffle",
            TypeBased(RuleOptions::WAFFLE),
            Equal,
            BindingModeBased({
                let mut c = Conf::waffle_2024();
                // Supporting the rule3 extension is too complicated.
                c.rule3_ext1 = false;
                c.rule3 = true;
                c
            }),
        ),
        (
            "rpjohnst",
            TypeBased(RuleOptions::RPJOHNST),
            Equal,
            BindingModeBased(Conf::rpjohnst_2024()),
        ),
        (
            "ergo2024_nonbreaking_transition_bm_based",
            BindingModeBased(Conf::rfc_3627_2024_min()),
            Less,
            BindingModeBased(Conf::rfc3627_2024()),
        ),
        (
            "ergo2024_nonbreaking_transition_type_based",
            TypeBased(RuleOptions::ERGO2024_BREAKING_ONLY),
            Less,
            TypeBased(RuleOptions::ERGO2024),
        ),
        (
            "minimal_breaking_transition_to_stateless",
            TypeBased(RuleOptions::ERGO2024_BREAKING_ONLY_EXT),
            Less,
            TypeBased(RuleOptions::STATELESS),
        ),
        (
            "minimal_breaking_transition_to_stateless_with_rule3",
            TypeBased(RuleOptions::ERGO2024_BREAKING_ONLY_EXT),
            Less,
            TypeBased(RuleOptions {
                downgrade_mut_inside_shared: true,
                ..RuleOptions::STATELESS
            }),
        ),
        (
            "minimal_breaking_transition_to_rfc3627_breaking",
            TypeBased(RuleOptions::ERGO2024_BREAKING_ONLY_EXT),
            Less,
            TypeBased(RuleOptions::ERGO2024_BREAKING_ONLY),
        ),
        (
            "minimal_breaking_transition_to_waffle",
            TypeBased(RuleOptions::ERGO2024_BREAKING_ONLY_EXT),
            Less,
            TypeBased(RuleOptions::WAFFLE),
        ),
        (
            "non_breaking_on_stable",
            TypeBased(RuleOptions::STABLE_RUST),
            Less,
            TypeBased(RuleOptions::RFC3627_2021),
        ),
        (
            "stable_vs_stateless",
            TypeBased(RuleOptions {
                ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
                mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
                ..RuleOptions::STABLE_RUST
            }),
            Equal,
            TypeBased(RuleOptions {
                ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
                mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
                allow_ref_pat_on_ref_mut: false,
                eat_inherited_ref_alone: false,
                ..RuleOptions::STATELESS
            }),
        ),
        (
            "rfc3627_vs_stateless",
            TypeBased(RuleOptions {
                ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
                mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
                allow_ref_pat_on_ref_mut: true,
                downgrade_mut_inside_shared: true,
                dont_eat_mut_inside_shared: true,
                ..RuleOptions::ERGO2024
            }),
            Equal,
            TypeBased(RuleOptions {
                ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
                mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
                allow_ref_pat_on_ref_mut: true,
                downgrade_mut_inside_shared: true,
                ..RuleOptions::STATELESS
            }),
        ),
        (
            "stateless_add_rule3",
            TypeBased(RuleOptions::STATELESS),
            Less,
            TypeBased(RuleOptions {
                downgrade_mut_inside_shared: true,
                ..RuleOptions::STATELESS
            }),
        ),
    ];

    let test_cases = TypingRequest::generate(a, 3, 4);
    let (shallow_test_cases, deep_test_cases): (Vec<_>, Vec<_>) =
        test_cases.into_iter().partition(|req| req.depth() <= 3);

    for &(name, left_ruleset, expected_order, right_ruleset) in compare {
        let mut trace = compare_rulesets(
            a,
            &shallow_test_cases,
            left_ruleset,
            expected_order,
            right_ruleset,
        )?;
        if trace.lines().count() <= 4 * 3 {
            // Try deeper patterns.
            trace = compare_rulesets(
                a,
                &deep_test_cases,
                left_ruleset,
                expected_order,
                right_ruleset,
            )?
        }

        if !trace.is_empty() {
            insta::with_settings!({
                snapshot_path => "../tests/snapshots",
                snapshot_suffix => format!("{name}"),
                prepend_module_to_snapshot => false,
                omit_expression => true,
                info => &name,
            }, {
                insta::assert_snapshot!(trace);
            });
        }
    }
    Ok(())
}
