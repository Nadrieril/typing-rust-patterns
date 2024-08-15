use itertools::Itertools;
use std::fmt::Display;

use crate::*;

impl Display for Mutable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Yes => f.write_str("mut "),
            Self::No => Ok(()),
        }
    }
}

impl Display for BindingMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ByMove => Ok(()),
            Self::ByRef(mutable) => write!(f, "ref {mutable}"),
        }
    }
}

impl Display for Pattern<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tuple(pats) => write!(f, "[{}]", pats.iter().format(", ")),
            Self::Ref(mutable, pat) => write!(f, "&{mutable}{pat}"),
            Self::Binding(mutable, mode, name) => write!(f, "{mutable}{mode}{name}"),
        }
    }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tuple(tys) => write!(f, "[{}]", tys.iter().format(", ")),
            Self::Ref(mutable, ty) => write!(f, "&{mutable}{ty}"),
            Self::Var(name) => write!(f, "{name}"),
        }
    }
}

impl Display for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Display for ExprKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ExprKind::Scrutinee => write!(f, "p"),
            ExprKind::Ref(mutable, e) => write!(f, "&{mutable}{e}"),
            ExprKind::Deref(e) => write!(f, "*{e}"),
            ExprKind::Field(e, n) => {
                let needs_parens = matches!(e.kind, ExprKind::Deref(..));
                if needs_parens {
                    write!(f, "({e}).{n}")
                } else {
                    write!(f, "{e}.{n}")
                }
            }
            ExprKind::CastAsImmRef(e) => write!(f, "({e} as &_)"),
        }
    }
}

impl Display for TypingRequest<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.pat, self.ty)
    }
}

impl Display for TypingPredicate<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // write!(f, "let {}: {} = {}", self.pat, self.expr.ty, self.expr)
        write!(f, "{} @ {}: {}", self.pat, self.expr, self.expr.ty)
    }
}
