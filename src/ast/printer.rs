use itertools::Itertools;
use std::fmt::{Debug, Display, Write};

use crate::*;

pub trait Style {
    fn green(&self) -> String;
    fn red(&self) -> String;
    fn comment(&self) -> String;
    fn tooltip(&self, text: &str) -> String;
    fn inherited_ref(&self) -> String;
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
    fn inherited_ref(&self) -> String {
        if cfg!(target_arch = "wasm32") {
            format!("<span class=\"inherited-ref\">{self}</span>")
        } else {
            use colored::Colorize;
            <Self as Colorize>::dimmed(self).to_string()
        }
        .tooltip("inherited reference")
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
    fn inherited_ref(&self) -> String {
        self.as_str().inherited_ref()
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
            PredicateStyle::Expression => {
                format!("{} @ {}: {}", self.pat, self.expr, self.expr.ty)
            }
            PredicateStyle::Sequent {
                ty: toi,
                show_reference_state,
                show_scrut_access,
            } => {
                // Bits of state before the turnstile.
                let mut pre_turnstile = vec![];
                if show_reference_state {
                    let bm = self.expr.binding_mode().ok();
                    let bm = match toi {
                        TypeOfInterest::UserVisible => match bm {
                            Some(BindingMode::ByRef(_)) => &"inh".inherited_ref().to_string(),
                            _ if !matches!(self.expr.ty, Type::Ref(..) | Type::Abstract(..)) => "_",
                            Some(BindingMode::ByMove) => "real",
                            None => "r",
                        },
                        TypeOfInterest::InMemory => match bm {
                            Some(bm) => bm.name(),
                            None => match self.expr.ty {
                                Type::Ref(Mutability::Shared, _) => "move or ref",
                                Type::Ref(Mutability::Mutable, _) => "move or ref mut",
                                Type::Abstract(_) => "bm",
                                _ => unreachable!(),
                            },
                        },
                    };
                    pre_turnstile.push(bm.to_string());
                }
                if show_scrut_access {
                    let scrut_access = match self.expr.scrutinee_mutability().ok() {
                        None => "m",
                        Some(Mutability::Mutable) => "rw",
                        Some(Mutability::Shared) => "ro",
                    };
                    pre_turnstile.push(scrut_access.to_string());
                }

                // Type to display.
                let ty = match toi {
                    TypeOfInterest::UserVisible => {
                        let mut ty = self.expr.ty.to_string();
                        if show_reference_state
                            && let Some(BindingMode::ByRef(_)) = self.expr.binding_mode().ok()
                        {
                            if let Some(rest) = ty.strip_prefix("&mut") {
                                ty = format!("{}{rest}", "&mut".inherited_ref());
                            } else if let Some(rest) = ty.strip_prefix("&") {
                                ty = format!("{}{rest}", "&".inherited_ref());
                            }
                        }
                        ty
                    }
                    TypeOfInterest::InMemory => match self.expr.binding_mode().ok() {
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
                    },
                };

                let turnstile = if pre_turnstile.is_empty() {
                    ""
                } else {
                    " ⊢ "
                };
                let pre_turnstile = pre_turnstile.join(", ");
                let pat = self.pat;
                format!("{pre_turnstile}{turnstile}{pat}: {ty}")
            }
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
                EatOuter | Error => {}
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
        match self.to_name() {
            Some(name) => write!(f, "{name}"),
            None => write!(f, "{self:?}"),
        }
    }
}

impl std::fmt::Display for BindingAssignments<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.assignments
                .iter()
                .map(|(name, ty)| format!("{name}: {ty}"))
                .format(", ")
        )
    }
}

impl std::fmt::Display for TypingResult<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let out = match self {
            TypingResult::Success(bindings) => format!("Success({bindings})").green(),
            TypingResult::BorrowError(bindings, s) => {
                format!("BorrowError({bindings}, \"{s:?}\")").red()
            }
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
