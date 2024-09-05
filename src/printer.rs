use itertools::Itertools;
use std::fmt::{Debug, Display};

use crate::*;

impl BindingMode {
    pub fn name(self) -> &'static str {
        match self {
            BindingMode::ByMove => "move",
            BindingMode::ByRef(Mutability::Shared) => "ref",
            BindingMode::ByRef(Mutability::Mutable) => "ref mut",
        }
    }

    pub fn as_borrow(self) -> &'static str {
        match self {
            BindingMode::ByMove => "",
            BindingMode::ByRef(Mutability::Shared) => "&",
            BindingMode::ByRef(Mutability::Mutable) => "&mut ",
        }
    }
}

impl<'a> TypingPredicate<'a> {
    /// Display as `let ...`.
    pub fn display_as_let(&self) -> String {
        format!("let {}: {} = {}", self.pat, self.expr.ty, self.expr)
    }

    pub fn display(&self, style: TypingRuleStyle) -> String {
        match style {
            TypingRuleStyle::Stateless => format!("{}: {}", self.pat, self.expr.ty),
            TypingRuleStyle::Sequent => {
                let bm = match self.expr.binding_mode().ok() {
                    None => "b",
                    Some(BindingMode::ByMove) => "place",
                    Some(BindingMode::ByRef(_)) => "value",
                };
                let scrut_access = match self.expr.scrutinee_mutability().ok() {
                    None => "m",
                    Some(Mutability::Mutable) => "rw",
                    Some(Mutability::Shared) => "ro",
                };
                format!("{bm}, {scrut_access} ⊢ {}: {}", self.pat, self.expr.ty)
            }
            _ => format!("{} @ {}: {}", self.pat, self.expr, self.expr.ty),
        }
    }
}

impl Display for Mutability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Mutable => f.write_str("mut "),
            Self::Shared => Ok(()),
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
        match *self {
            Self::Tuple(pats) => write!(f, "[{}]", pats.iter().format(", ")),
            Self::Ref(mutable, pat) => {
                let needs_parens = mutable == Mutability::Shared
                    && matches!(pat, Self::Binding(Mutability::Mutable, ..));
                if needs_parens {
                    write!(f, "&{mutable}({pat})")
                } else {
                    write!(f, "&{mutable}{pat}")
                }
            }
            Self::Binding(mutable, mode, name) => write!(f, "{mutable}{mode}{name}"),
            Self::Abstract(name) => write!(f, "{name}"),
        }
    }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Tuple(tys) => write!(f, "[{}]", tys.iter().format(", ")),
            Self::Ref(mutable, ty) => write!(f, "&{mutable}{ty}"),
            Self::NonRef(name) => write!(f, "{name}"),
            Self::Abstract(name) => write!(f, "{name}"),
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
            ExprKind::Scrutinee => write!(f, "s"),
            ExprKind::Abstract { .. } => write!(f, "e"),
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
        write!(f, "{}", self.display(TypingRuleStyle::Expression))
    }
}

impl Display for TypingRuleStyle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", serde_yaml::to_string(self).unwrap().trim())
    }
}

impl Debug for Mutability {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl Debug for BindingMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl Debug for Pattern<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl Debug for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl Debug for Expression<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl Debug for ExprKind<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl Debug for TypingRequest<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}

impl Debug for TypingPredicate<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
