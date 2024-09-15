use std::fmt::Write;

use crate::*;
pub use match_ergonomics_formality::Conf;
use match_ergonomics_formality::Reduction;
use TypingResult::Success;

mod convert {
    use crate::Mutability::*;
    use match_ergonomics_formality::*;
    fn convert_pattern(pat: &crate::Pattern<'_>) -> Pattern {
        let span = Span::nop();
        match pat {
            crate::Pattern::Abstract(_) => {
                panic!("cannot convert abstract pattern to match_ergonomics_formality")
            }
            crate::Pattern::Tuple(pats) => {
                assert_eq!(
                    pats.len(),
                    1,
                    "only length-1 tuples are supported by match_ergonomics_formality"
                );
                let pat = convert_pattern(&pats[0]);
                Pattern::Slice(SlicePat::new(pat, span, span))
            }
            crate::Pattern::Ref(mtbl, pat) => {
                let pat = convert_pattern(pat);
                match mtbl {
                    Shared => Pattern::Ref(RefPat::new(pat, span, span)),
                    Mutable => Pattern::RefMut(RefMutPat::new(pat, span, span)),
                }
            }
            crate::Pattern::Binding(mtbl, bm, name) => Pattern::Binding(BindingPat {
                ident: Ident::new(name.to_string(), span),
                mode: match bm {
                    crate::BindingMode::ByMove => BindingMode::Move,
                    crate::BindingMode::ByRef(Shared) => BindingMode::Ref,
                    crate::BindingMode::ByRef(Mutable) => BindingMode::RefMut,
                },
                is_mut: matches!(mtbl, Mutable),
                span,
            }),
        }
    }

    fn convert_type(ty: &crate::Type<'_>) -> Expr {
        let span = Span::nop();
        match ty {
            crate::Type::Abstract(_) | crate::Type::AbstractNonRef(_) => {
                panic!("cannot convert abstract type to match_ergonomics_formality")
            }
            crate::Type::Tuple(tys) => {
                assert_eq!(
                    tys.len(),
                    1,
                    "only length-1 tuples are supported by match_ergonomics_formality"
                );
                let ty = convert_type(&tys[0]);
                Expr::Slice(SliceExpr::new(ty, span, span))
            }
            crate::Type::Ref(mtbl, ty) => {
                let ty = convert_type(ty);
                match mtbl {
                    Shared => Expr::Ref(RefExpr::new(ty, span, span)),
                    Mutable => Expr::RefMut(RefMutExpr::new(ty, span, span)),
                }
            }
            crate::Type::OtherNonRef(name) => Expr::Type(TypeExpr {
                name: Ident::new(name.to_string(), span),
                span,
            }),
        }
    }

    impl<'a> crate::Type<'a> {
        pub fn from_bm_based(a: &'a crate::Arenas<'a>, ty: &Expr) -> Self {
            unconvert_type(a, ty)
        }
    }

    fn unconvert_type<'a>(a: &'a crate::Arenas<'a>, ty: &Expr) -> crate::Type<'a> {
        match ty {
            Expr::Type(ty) => crate::Type::OtherNonRef(a.bump.alloc_str(&ty.name.name)),
            Expr::RefMut(ty) => crate::Type::Ref(Mutable, unconvert_type(a, &ty.expr).alloc(a)),
            Expr::Ref(ty) => crate::Type::Ref(Shared, unconvert_type(a, &ty.expr).alloc(a)),
            Expr::Slice(ty) => {
                crate::Type::Tuple(std::slice::from_ref(unconvert_type(a, &ty.expr).alloc(a)))
            }
            Expr::Paren(ty) => unconvert_type(a, &ty.expr),
        }
    }

    fn convert_request(req: &crate::TypingRequest<'_>) -> LetStmt {
        let span = Span::nop();
        let pat = convert_pattern(req.pat);
        let ty = convert_type(req.ty);
        LetStmt::new(pat, ty, span, span)
    }

    impl<'a> crate::TypingRequest<'a> {
        pub fn to_bm_based(&self) -> LetStmt {
            convert_request(self)
        }
    }
}

pub fn run_formality<'a>(
    a: &'a Arenas<'a>,
    conf: Conf,
    req: &TypingRequest<'a>,
    mut callback: impl FnMut(&Reduction),
) -> TypingResult<'a> {
    if req.pat.contains_abstract() {
        return TypingResult::TypeError(TypeError::OverlyGeneral(DeepeningRequest::Pattern));
    } else if req.ty.contains_abstract() {
        return TypingResult::TypeError(TypeError::OverlyGeneral(DeepeningRequest::Type));
    }

    let stmt = req.to_bm_based();
    let mut r = Reduction::from_stmt(conf, stmt);
    while !r.last {
        callback(&r);
        r.step();
    }

    match r.node_step.error {
        Some(e) => TypingResult::TypeError(TypeError::External(e)),
        None => {
            let (ident, ty) = r.as_type();
            let ty = Type::from_bm_based(a, &ty);
            let name = a.alloc_str(&ident.name);
            let bindings = BindingAssignments::new([(name, ty)]);
            let out = Success(bindings);
            // Just for printing, feels risky to do before `as_type`.
            r.apply_dbm();
            callback(&r);
            out
        }
    }
}

pub fn trace_with_formality<'a>(conf: Conf, req: &TypingRequest<'a>) -> String {
    let a = &Arenas::default();
    let mut out = String::new();
    run_formality(a, conf, req, |r| {
        let _ = write!(&mut out, "{}", r);
    });
    out
}

pub fn typecheck_with_formality<'a>(
    a: &'a Arenas<'a>,
    conf: Conf,
    req: &TypingRequest<'a>,
) -> TypingResult<'a> {
    run_formality(a, conf, req, |_| {})
}
