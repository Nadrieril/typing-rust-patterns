use crate::*;
use itertools::Itertools;
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
    TypeError(String),
}

impl<'a> AnalysisResult<'a> {
    pub fn matches(&self, other: &Self) -> bool {
        match (self, other) {
            (Success(lty), Success(rty)) => lty == rty,
            (TypeError(_), TypeError(_)) => true,
            _ => false,
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
            match pred.expr.simplify(ctx).borrow_check() {
                // This error isn't handled by `match-ergo-formality` so we ignore it.
                Ok(()) | Err(BorrowCheckError::CantCopyNestedRefMut) => Success(*pred.expr.ty),
                Err(err) => TypeError(format!("{err:?}")),
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

/// Automatically generate concrete types up to a given depth.
pub fn generate_types<'a>(a: &'a Arenas<'a>, depth: usize) -> Vec<&'a Type<'a>> {
    pub fn generate_types_inner<'a>(a: &'a Arenas<'a>, depth: usize) -> Vec<&'a Type<'a>> {
        if depth == 0 {
            return vec![&Type::Abstract("T")];
        }
        let base_ty = Type::NonRef("T");
        generate_types_inner(a, depth - 1)
            .into_iter()
            .flat_map(|ty| {
                [ty.subst(a, base_ty)]
                    .into_iter()
                    .chain(ty.deepen(a, false))
                    .map(|ty| ty.alloc(a))
            })
            .collect()
    }
    let base_ty = Type::NonRef("T");
    generate_types_inner(a, depth)
        .into_iter()
        .map(|ty| ty.subst(a, base_ty))
        .map(|ty| ty.alloc(a))
        .collect()
}

/// Automatically generate concrete types up to a given depth.
pub fn generate_patterns<'a>(a: &'a Arenas<'a>, depth: usize) -> Vec<&'a Pattern<'a>> {
    pub fn generate_patterns_inner<'a>(a: &'a Arenas<'a>, depth: usize) -> Vec<&'a Pattern<'a>> {
        if depth == 0 {
            return vec![&Pattern::Abstract("p")];
        }
        generate_patterns_inner(a, depth - 1)
            .into_iter()
            .flat_map(|pat| {
                pat.deepen(a, false)
                    .into_iter()
                    .map(|pat| pat.alloc(a))
                    .chain((!pat.contains_abstract()).then_some(pat))
            })
            .collect()
    }
    let base_pat = Pattern::Binding(Mutability::Shared, BindingMode::ByMove, "x");
    generate_patterns_inner(a, depth)
        .into_iter()
        .map(|pat| pat.subst(a, base_pat))
        .map(|pat| pat.alloc(a))
        .collect()
}

/// Automatically generate many requests.
pub fn generate_requests<'a>(a: &'a Arenas<'a>, depth: usize) -> Vec<TypingRequest<'a>> {
    let patterns = generate_patterns(a, depth);
    let types = generate_types(a, depth);
    patterns
        .iter()
        .cartesian_product(types)
        .map(|(pat, ty)| TypingRequest { pat, ty })
        .collect()
}
