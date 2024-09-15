use itertools::Itertools;
use std::fmt::{Debug, Display, Write};

use crate::*;

pub trait Style {
    fn green(&self) -> String;
    fn red(&self) -> String;
    fn dimmed(&self) -> String;
    fn comment(&self) -> String;
    fn tooltip(&self, text: &str) -> String;
    fn code(&self) -> String;
}

impl Style for &str {
    fn green(&self) -> String {
        if cfg!(target_arch = "wasm32") {
            format!("<span style=\"color: green\">{self}</span>")
        } else {
            use colored::Colorize;
            <Self as Colorize>::green(self).to_string()
        }
    }
    fn red(&self) -> String {
        if cfg!(target_arch = "wasm32") {
            format!("<span style=\"color: red\">{self}</span>")
        } else {
            use colored::Colorize;
            <Self as Colorize>::red(self).to_string()
        }
    }
    fn dimmed(&self) -> String {
        if cfg!(target_arch = "wasm32") {
            format!("<span style=\"color: gray\">{self}</span>")
        } else {
            use colored::Colorize;
            <Self as Colorize>::dimmed(self).to_string()
        }
    }
    fn comment(&self) -> String {
        if cfg!(target_arch = "wasm32") {
            format!("<span style=\"color: dimgray\">{self}</span>")
        } else {
            use colored::Colorize;
            <Self as Colorize>::dimmed(self).to_string()
        }
    }
    fn tooltip(&self, text: &str) -> String {
        if cfg!(target_arch = "wasm32") {
            format!("<span title=\"{text}\">{self}</span>")
        } else {
            self.to_string()
        }
    }
    fn code(&self) -> String {
        if cfg!(target_arch = "wasm32") {
            format!("<code>{self}</code>")
        } else {
            format!("`{self}`")
        }
    }
}

impl Style for String {
    fn green(&self) -> String {
        self.as_str().green()
    }
    fn red(&self) -> String {
        self.as_str().red()
    }
    fn dimmed(&self) -> String {
        self.as_str().dimmed()
    }
    fn comment(&self) -> String {
        self.as_str().comment()
    }
    fn tooltip(&self, text: &str) -> String {
        self.as_str().tooltip(text)
    }
    fn code(&self) -> String {
        self.as_str().code()
    }
}

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

    pub fn display(&self, style: PredicateStyle) -> String {
        match style {
            PredicateStyle::Stateless => format!("{}: {}", self.pat, self.expr.ty),
            PredicateStyle::Sequent => {
                let mut ty = self.expr.ty.to_string();
                let bm = match self.expr.binding_mode().ok() {
                    Some(BindingMode::ByRef(_)) => {
                        if let Some(rest) = ty.strip_prefix("&mut") {
                            ty =
                                format!("{}{rest}", "&mut".dimmed().tooltip("inherited reference"));
                        } else if let Some(rest) = ty.strip_prefix("&") {
                            ty = format!("{}{rest}", "&".dimmed().tooltip("inherited reference"));
                        }
                        &"inh".dimmed().to_string()
                    }
                    _ if !matches!(self.expr.ty, Type::Ref(..) | Type::Abstract(..)) => "_",
                    Some(BindingMode::ByMove) => "real",
                    None => "r",
                };
                let scrut_access = match self.expr.scrutinee_mutability().ok() {
                    None => "m",
                    Some(Mutability::Mutable) => "rw",
                    Some(Mutability::Shared) => "ro",
                };
                format!("{bm}, {scrut_access} ⊢ {}: {ty}", self.pat)
            }
            PredicateStyle::SequentBindingMode => {
                let bm = self.expr.binding_mode().ok();
                let scrut_access = match self.expr.scrutinee_mutability().ok() {
                    None => "m",
                    Some(Mutability::Mutable) => "rw",
                    Some(Mutability::Shared) => "ro",
                };
                let ty = match bm {
                    Some(BindingMode::ByMove) => self.expr.ty.to_string(),
                    Some(BindingMode::ByRef(_)) => {
                        self.expr.reset_binding_mode().unwrap().ty.to_string()
                    }
                    None => match self.expr.ty {
                        Type::Ref(_, inner_ty) => {
                            format!("{} or {}", self.expr.ty, inner_ty)
                        }
                        Type::Abstract(_) => self.expr.ty.to_string(),
                        _ => unreachable!(),
                    },
                };
                let bm = match bm {
                    None => match self.expr.ty {
                        Type::Ref(Mutability::Shared, _) => "move or ref",
                        Type::Ref(Mutability::Mutable, _) => "move or ref mut",
                        Type::Abstract(_) => "bm",
                        _ => unreachable!(),
                    },
                    Some(bm) => bm.name(),
                };
                format!("{bm}, {scrut_access} ⊢ {}: {ty}", self.pat)
            }
            _ => format!("{} @ {}: {}", self.pat, self.expr, self.expr.ty),
        }
    }
}

impl Rule {
    pub fn display(&self, options: RuleOptions) -> String {
        use DowngradeMutToRef::*;
        use InheritedRefOnRefBehavior::*;
        use Rule::*;
        let debug = format!("{self:?}");
        let variant_name = debug.split("(").next().unwrap_or(&debug);

        // Only display extra info if it can change.
        let mut extras = vec![];
        if let Deref(x, _, _) | DerefMutWithShared(x) = *self {
            match options.inherited_ref_on_ref {
                EatOuter => {}
                EatBoth | EatInner => extras.push(format!("{x:?}")),
            }
        }
        if let ConstructorRef(x) | ConstructorMultiRef(x) | Deref(_, x, _) = *self
            && options.downgrade_mut_inside_shared
            && x == ForceReadOnly
        {
            extras.push(format!("{x:?}"))
        }
        if let Deref(_, _, x) = *self
            && options.fallback_to_outer
            && x == FallbackToOuter(true)
        {
            extras.push(format!("FallbackToOuter"))
        }

        let mut out = variant_name.to_string();
        if !extras.is_empty() {
            let _ = write!(&mut out, "({})", extras.iter().format(", "));
        }
        out
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
            Self::OtherNonRef(name) | Self::AbstractNonRef(name) | Self::Abstract(name) => {
                write!(f, "{name}")
            }
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
        write!(f, "{}", self.display(PredicateStyle::Expression))
    }
}

impl Display for PredicateStyle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", serde_yaml::to_string(self).unwrap().trim())
    }
}

impl std::fmt::Display for TypingResult<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = match self {
            TypingResult::Success(ty) => format!("Success({ty})").green(),
            TypingResult::BorrowError(ty, s) => format!("BorrowError({ty:?}, \"{s:?}\")").red(),
            TypingResult::TypeError(TypeError::External(e)) => format!("TypeError(\"{e}\")").red(),
            TypingResult::TypeError(e) => format!("TypeError(\"{e:?}\")").red(),
        };
        write!(f, "{}", out)
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
