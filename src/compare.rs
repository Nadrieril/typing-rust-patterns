use crate::*;
use match_ergonomics_formality::Conf;

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
    pub fn matches(&self, other: &Self) -> bool {
        match (self, other) {
            // These borrow errors only come from this solver, whose borrow checker is more
            // accurate. Hence if both agree on types we ignore the borrowck error.
            (Success(lty), Success(rty))
            | (BorrowError(lty, _), Success(rty))
            | (Success(lty), BorrowError(rty, _)) => lty == rty,
            (TypeError(_) | BorrowError(..), TypeError(_) | BorrowError(..)) => true,
            (Success(_), TypeError(_)) | (TypeError(_), Success(_)) => false,
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

#[test]
/// Compare rulesets with the `ergo-formality` reference implementation.
fn compare() -> anyhow::Result<()> {
    use anyhow::Context;
    use std::fmt::Write;

    let a = &Arenas::default();
    let compare = [
        (
            "default",
            RuleOptions {
                // `ergo-formality` doesn't support the `Keep` option.
                mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
                ..RuleOptions::DEFAULT
            },
            {
                let mut c = Conf::default();
                c.rule1 = true;
                c.rule4_early = true;
                c.rule5 = true;
                c
            },
        ),
        ("stable_rust", RuleOptions::STABLE_RUST, Conf::rfc2005()),
        ("structural", RuleOptions::STRUCTURAL, Conf::pre_rfc2005()),
        ("ergo2024", RuleOptions::ERGO2024, Conf::rfc3627_2024()),
        (
            "rfc3627_2021",
            RuleOptions::RFC3627_2021,
            Conf::rfc3627_2021(),
        ),
        (
            "ergo2024_breaking_only",
            RuleOptions::ERGO2024_BREAKING_ONLY,
            Conf::rfc_3627_2024_min(),
        ),
        ("waffle", RuleOptions::WAFFLE, {
            let mut c = Conf::waffle_2024();
            // Supporting the rule3 extension is too complicated.
            c.rule3_ext1 = false;
            c.rule3 = true;
            c
        }),
        ("rpjohnst", RuleOptions::RPJOHNST, Conf::rpjohnst_2024()),
    ];

    let test_cases = TypingRequest::generate(a, 3, 4);

    for (name, ty_based, bm_based) in compare {
        let ty_based = RuleSet::TypeBased(ty_based);
        let bm_based = RuleSet::BindingModeBased(bm_based);

        let mut trace = String::new();
        for test_case in &test_cases {
            let test_case_str = test_case.to_string();
            let left_res = &ty_based
                .analyze(a, *test_case)
                .context(test_case_str.clone())?;
            let right_res = &bm_based
                .analyze(a, *test_case)
                .context(test_case_str.clone())?;
            if left_res.matches(right_res) {
                continue;
            }
            let _ = writeln!(&mut trace, "Difference on `{test_case_str}`:");
            let _ = writeln!(&mut trace, "  type-based returned: {left_res:?}");
            let _ = writeln!(&mut trace, "    bm-based returned: {right_res:?}");
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
