use itertools::Itertools;
use std::fmt::{Debug, Display, Write};

use crate::*;

pub mod display_tree;
pub use display_tree::*;

pub trait Style: Display + AsRef<str> {
    fn green(&self) -> String;
    fn red(&self) -> String;
    fn comment(&self) -> String;
    fn dimmed(&self) -> String;
    fn tooltip(&self, text: &str) -> String;
    fn inherited_ref(&self) -> String;
    fn code(&self) -> String;

    fn wrap_in_tag(&self, tag_name: &str, tag_args: Option<(&str, &str)>) -> String {
        let tag_args = tag_args
            .map(|(k, v)| format!("{k}=\"{v}\""))
            .unwrap_or_default();
        format!("<{tag_name} {tag_args}>{self}</{tag_name}>")
    }
    fn span_style(&self, style: &str) -> String {
        self.wrap_in_tag("span", Some(("style", style)))
    }
    fn apply_colorize<'a>(&'a self, f: impl Fn(&'a str) -> colored::ColoredString) -> String {
        // Apply line-by-line so that we can split by line later without messing up escape codes.
        self.as_ref().lines().map(|line| f(line)).join("\n")
    }
}

impl<T: Display + AsRef<str>> Style for T {
    fn green(&self) -> String {
        if cfg!(target_arch = "wasm32") {
            self.span_style("color: green")
        } else {
            use colored::Colorize;
            self.apply_colorize(<_ as Colorize>::green)
        }
    }
    fn red(&self) -> String {
        if cfg!(target_arch = "wasm32") {
            self.span_style("color: red")
        } else {
            use colored::Colorize;
            self.apply_colorize(<_ as Colorize>::red)
        }
    }
    fn dimmed(&self) -> String {
        if cfg!(target_arch = "wasm32") {
            self.span_style("opacity: 0.5")
        } else {
            use colored::Colorize;
            self.apply_colorize(<_ as Colorize>::dimmed)
        }
    }
    fn comment(&self) -> String {
        if cfg!(target_arch = "wasm32") {
            self.span_style("color: dimgray")
        } else {
            use colored::Colorize;
            self.apply_colorize(<_ as Colorize>::dimmed)
        }
    }
    fn tooltip(&self, text: &str) -> String {
        if cfg!(target_arch = "wasm32") {
            self.wrap_in_tag("span", Some(("title", text)))
        } else {
            self.to_string()
        }
    }
    fn inherited_ref(&self) -> String {
        if cfg!(target_arch = "wasm32") {
            self.wrap_in_tag("span", Some(("class", "inherited-ref")))
        } else {
            use colored::Colorize;
            self.apply_colorize(<_ as Colorize>::dimmed)
        }
        .tooltip("inherited reference")
    }
    fn code(&self) -> String {
        if cfg!(target_arch = "wasm32") {
            self.wrap_in_tag("code", None)
        } else {
            format!("`{self}`")
        }
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

/// TODO: Currently displayed as a single leaf.
impl<'d> ToDisplayTree<'d> for Pattern<'_> {
    fn to_display_tree(&self, a: &'d Arenas<'d>) -> DisplayTree<'d> {
        match *self {
            Self::Tuple(pats) => DisplayTree::sep_by(a, ", ", pats.iter())
                .surrounded(a, "[", "]")
                .tag("pat_list"),
            Self::Ref(mutable, pat) => {
                let needs_parens = mutable == Mutability::Shared
                    && matches!(pat, Self::Binding(Mutability::Mutable, ..));
                let (before, after) = if needs_parens {
                    (format!("&{mutable}("), ")")
                } else {
                    (format!("&{mutable}"), "")
                };
                DisplayTree::sep_by(
                    a,
                    "",
                    [
                        before.to_display_tree(a),
                        pat.to_display_tree(a),
                        after.to_display_tree(a),
                    ],
                )
            }
            Self::Binding(mutable, mode, name) => {
                DisplayTree::sep_by(a, "", [&mutable.to_string(), &mode.to_string(), name])
            }
            Self::Abstract(name) => name.to_display_tree(a),
        }
    }
}

impl<'d> ToDisplayTree<'d> for Type<'_> {
    fn to_display_tree(&self, a: &'d Arenas<'d>) -> DisplayTree<'d> {
        match self {
            Self::Tuple(tys) => DisplayTree::sep_by(a, ", ", tys.iter())
                .surrounded(a, "[", "]")
                .tag("ty_list"),
            Self::Ref(mutable, ty) => format!("&{mutable}")
                .to_display_tree(a)
                .then(a, ty)
                .tag("ty_ref"),
            Self::OtherNonRef(name) | Self::AbstractNonRef(name) | Self::Abstract(name) => {
                name.to_display_tree(a)
            }
        }
    }
}

impl<'d> ToDisplayTree<'d> for BindingAssignments<'_> {
    fn to_display_tree(&self, a: &'d Arenas<'d>) -> DisplayTree<'d> {
        DisplayTree::sep_by(
            a,
            ", ",
            self.assignments
                .iter()
                .map(|(name, ty)| name.to_display_tree(a).sep_then(a, ": ", ty)),
        )
    }
}

impl<'d> ToDisplayTree<'d> for TypingResult<'_> {
    fn to_display_tree(&self, a: &'d Arenas<'d>) -> DisplayTree<'d> {
        match self {
            TypingResult::Success(bindings) => {
                bindings.to_display_tree(a).surrounded(a, "Success(", ")")
            }
            TypingResult::BorrowError(bindings, s) => bindings
                .to_display_tree(a)
                .sep_then(a, ", ", format!("\"{s:?}\""))
                .surrounded(a, "BorrowError(", ")"),
            TypingResult::TypeError(TypeError::External(e)) => format!("{e}")
                .to_display_tree(a)
                .surrounded(a, "TypeError(\"", "\")"),
            TypingResult::TypeError(e) => {
                format!("{e:?}")
                    .to_display_tree(a)
                    .surrounded(a, "TypeError(\"", "\")")
            }
        }
    }
}

impl TypingResult<'_> {
    pub fn display(&self) -> String {
        let a = &Arenas::default();
        let out = self.to_display_tree(a).to_string();
        match self {
            TypingResult::Success(..) => out.green(),
            TypingResult::BorrowError(..) | TypingResult::TypeError(..) => out.red(),
        }
    }

    /// Display two typing results, adjusting colors to highlight differences.
    pub fn display_diffed(&self, other: &Self) -> (String, String) {
        let a = &Arenas::default();
        match (self, other) {
            (TypingResult::Success(..), TypingResult::Success(..)) => {
                let left = self.to_display_tree(a);
                let right = other.to_display_tree(a);
                left.diff_display(&right)
            }
            _ => (self.to_string(), other.to_string()),
        }
    }
}

impl<'a> TypingPredicate<'a> {
    /// Display as `let ...`.
    pub fn display_as_let<'d>(&self, a: &'d Arenas<'d>) -> DisplayTree<'d> {
        self.pat
            .to_display_tree(a)
            .sep_then(a, ": ", self.expr.ty)
            .sep_then(a, " = ", self.expr.to_string())
            .preceded(a, "let ")
    }

    pub fn display(&self, style: PredicateStyle) -> String {
        let a = &Arenas::default();
        self.display_to_tree(a, style).to_string()
    }

    /// Display according to the given predicate style.
    pub fn display_to_tree<'d>(&self, a: &'d Arenas<'d>, style: PredicateStyle) -> DisplayTree<'d> {
        match style {
            PredicateStyle::Expression => self
                .pat
                .to_display_tree(a)
                .sep_then(a, " @ ", self.expr.to_string())
                .sep_then(a, ": ", self.expr.ty),
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
                let pre_turnstile = DisplayTree::sep_by(a, ", ", pre_turnstile);

                // Type to display.
                let ty = match toi {
                    TypeOfInterest::UserVisible => {
                        let ty = self.expr.ty;
                        if show_reference_state
                            && let Some(BindingMode::ByRef(_)) = self.expr.binding_mode().ok()
                            && let Type::Ref(mtbl, sub_ty) = ty
                        {
                            // Highlight the inherited reference.
                            format!("&{mtbl}")
                                .inherited_ref()
                                .to_display_tree(a)
                                .then(a, sub_ty)
                                .tag("ty_ref")
                        } else {
                            ty.to_display_tree(a)
                        }
                    }
                    TypeOfInterest::InMemory => match self.expr.binding_mode().ok() {
                        Some(BindingMode::ByMove) => self.expr.ty.to_display_tree(a),
                        Some(BindingMode::ByRef(_)) => self
                            .expr
                            .reset_binding_mode()
                            .unwrap()
                            .ty
                            .to_display_tree(a),
                        None => match self.expr.ty {
                            Type::Ref(_, inner_ty) => {
                                format!("{} or {}", self.expr.ty, inner_ty).to_display_tree(a)
                            }
                            Type::Abstract(_) => self.expr.ty.to_display_tree(a),
                            _ => unreachable!(),
                        },
                    },
                };
                let post_turnstile = self.pat.to_display_tree(a).sep_then(a, ": ", ty);

                let parts: &[_] = if pre_turnstile.is_empty() {
                    &[post_turnstile]
                } else {
                    &[pre_turnstile, post_turnstile]
                };
                DisplayTree::sep_by(a, " ⊢ ", parts)
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
        let a = &Arenas::default();
        write!(f, "{}", self.to_display_tree(a))
    }
}

impl Display for Type<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let a = &Arenas::default();
        write!(f, "{}", self.to_display_tree(a))
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

impl Display for TypingResult<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.display())
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
