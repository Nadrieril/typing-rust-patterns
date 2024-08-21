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
