use std::fmt::Display;

use itertools::Itertools;

use crate::*;
use DisplayTreeKind::*;

/// A structured representation of a string to be displayed. Used to compute structured diffs.
#[derive(Clone, Copy)]
enum DisplayTreeKind<'a> {
    /// Leaf node.
    Leaf(&'a str),
    /// Separated node. Considered different if the separator is different. If the lengths are
    /// different, different options are possible.
    Separated {
        /// The separator, intercalated between the children.
        sep: &'a str,
        /// The children.
        children: &'a [DisplayTree<'a>],
        /// If the lengths differ and this is `true`, the first elements are diffed together until
        /// exhaustion. If the lengths differ and this is `false`, we consider the whole subtree to
        /// differ.
        compare_common_prefix: bool,
    },
}

#[derive(Clone, Copy)]
pub struct DisplayTree<'a> {
    kind: DisplayTreeKind<'a>,
    /// Identifies the kind of node. Two nodes with different tags are always considered different.
    tag: &'static str,
    /// Whether to consider this sub-tree unchanged for the purposes of diffing.
    ignore_for_diff: bool,
}

/// Compute the length of this string as displayed on the screen (i.e. ignoring html tags or ansi
/// escapes, depending on context).
pub(crate) fn len_ignoring_markup(s: &str) -> usize {
    if cfg!(target_arch = "wasm32") {
        // Compute string length skipping html tags.
        let mut in_tag = false;
        s.chars()
            .filter(|&c| {
                if c == '<' {
                    in_tag = true;
                    false
                } else if c == '>' {
                    in_tag = false;
                    false
                } else {
                    !in_tag
                }
            })
            .count()
    } else {
        // Compute string length skipping ansi escape codes.
        ansi_width::ansi_width(s)
    }
}

/// Strip html tags or ansi escapes, depending on context.
fn strip_markup(s: &str) -> String {
    if cfg!(target_arch = "wasm32") {
        let mut in_tag = false;
        s.chars()
            .filter(|&c| {
                if c == '<' {
                    in_tag = true;
                    false
                } else if c == '>' {
                    in_tag = false;
                    false
                } else {
                    !in_tag
                }
            })
            .collect()
    } else {
        strip_ansi_escapes::strip_str(s)
    }
}

impl<'a> DisplayTree<'a> {
    fn new_from_kind(kind: DisplayTreeKind<'a>) -> Self {
        Self {
            kind,
            tag: "",
            ignore_for_diff: false,
        }
    }

    pub fn is_empty(&self) -> bool {
        match self.kind {
            Leaf(s) => s.is_empty(),
            Separated { children, .. } => children.is_empty(),
        }
    }

    pub fn len_ignoring_markup(&self) -> usize {
        match self.kind {
            Leaf(s) => len_ignoring_markup(s),
            Separated { sep, children, .. } => {
                let sep_len = if children.len() <= 1 {
                    0
                } else {
                    len_ignoring_markup(sep) * (children.len() - 1)
                };
                let children_len: usize = children.iter().map(|t| t.len_ignoring_markup()).sum();
                sep_len + children_len
            }
        }
    }

    pub fn leaf_noalloc(s: &'a str) -> Self {
        Self::new_from_kind(Leaf(s))
    }

    pub fn leaf(a: &'a Arenas<'a>, s: &str) -> Self {
        Self::leaf_noalloc(a.alloc_str(s))
    }

    fn mk_separated(
        a: &'a Arenas<'a>,
        sep: &str,
        children: impl IntoIterator<Item: ToDisplayTree<'a>>,
        compare_common_prefix: bool,
    ) -> Self {
        let children = children
            .into_iter()
            .map(|x| x.to_display_tree(a))
            .collect_vec();
        Self::new_from_kind(Separated {
            sep: a.alloc_str(sep),
            children: a.bump.alloc_slice_copy(&children),
            compare_common_prefix,
        })
    }

    pub fn sep_by(
        a: &'a Arenas<'a>,
        sep: &str,
        children: impl IntoIterator<Item: ToDisplayTree<'a>>,
    ) -> Self {
        Self::mk_separated(a, sep, children, false)
    }

    pub fn sep_by_compare_prefix(
        a: &'a Arenas<'a>,
        sep: &str,
        children: impl IntoIterator<Item: ToDisplayTree<'a>>,
    ) -> Self {
        Self::mk_separated(a, sep, children, true)
    }

    /// Concatenates `self` and `x`, separated by `sep`.
    pub fn sep_then(
        &self,
        a: &'a Arenas<'a>,
        sep: &'static str,
        x: impl ToDisplayTree<'a>,
    ) -> Self {
        Self::sep_by(a, sep, [self.to_display_tree(a), x.to_display_tree(a)])
    }

    /// Constructs `self` followed by `after`.
    pub fn then(&self, a: &'a Arenas<'a>, x: impl ToDisplayTree<'a>) -> Self {
        self.sep_then(a, "", x)
    }

    /// Constructs `self` surrounded by `before` and `after`.
    pub fn preceded(&self, a: &'a Arenas<'a>, before: &'static str) -> Self {
        Self::sep_by(a, "", [Self::leaf_noalloc(before), *self])
    }

    /// Constructs `self` surrounded by `before` and `after`.
    pub fn surrounded(&self, a: &'a Arenas<'a>, before: &'static str, after: &'static str) -> Self {
        Self::sep_by(
            a,
            "",
            [Self::leaf_noalloc(before), *self, Self::leaf_noalloc(after)],
        )
    }

    pub fn ignore_for_diff(mut self) -> Self {
        self.ignore_for_diff = true;
        self
    }

    pub fn tag(mut self, tag: &'static str) -> Self {
        self.tag = tag;
        self
    }

    /// Display `self` and `other`, highlighting differences.
    pub fn diff_display(&self, other: &Self) -> (String, String) {
        let (left, right, _) = self.diff_display_has_diff(other);
        (left, right)
    }

    /// Display `self` and `other`, highlighting differences. Returns whether there was any diff.
    pub fn diff_display_has_diff(&self, other: &Self) -> (String, String, bool) {
        let mut left = String::new();
        let mut right = String::new();
        let has_diff = self
            .diff_display_inner(other, &mut left, &mut right)
            .unwrap();
        (left, right, has_diff)
    }

    /// Returns whether there was any diff.
    fn diff_display_inner(
        &self,
        other: &Self,
        left: &mut String,
        right: &mut String,
    ) -> Result<bool, std::fmt::Error> {
        use std::fmt::Write;
        // The trivial cases: the trees are either fully identical or fully different.
        let all_same = |left: &mut String, right: &mut String| {
            write!(left, "{self}")?;
            write!(right, "{other}")?;
            Ok(false)
        };
        let all_different = |left: &mut String, right: &mut String| {
            write!(left, "{}", self.to_string().red())?;
            write!(right, "{}", other.to_string().green())?;
            Ok(true)
        };
        match (self.kind, other.kind) {
            _ if self.tag != other.tag => all_different(left, right),
            _ if self.ignore_for_diff && other.ignore_for_diff => all_same(left, right),
            (Leaf(l), Leaf(r)) if strip_markup(l) == strip_markup(r) => all_same(left, right),
            // The non-trivial case: the trees differ partially.
            (
                Separated {
                    sep,
                    children: c1,
                    compare_common_prefix: ccp1,
                },
                Separated {
                    sep: sep2,
                    children: c2,
                    compare_common_prefix: ccp2,
                },
            ) if strip_markup(sep) == strip_markup(sep2)
                && (c1.len() == c2.len() || ccp1 || ccp2) =>
            {
                let mut is_first = true;
                let mut any_diff = false;
                for either_or_both in c1.iter().copied().zip_longest(c2.iter().copied()) {
                    if !is_first && !either_or_both.is_right() {
                        write!(left, "{sep}")?;
                    }
                    if !is_first && !either_or_both.is_left() {
                        write!(right, "{sep}")?;
                    }
                    let (c1, c2) = either_or_both.or_default();
                    any_diff |= c1.diff_display_inner(&c2, left, right)?;
                    is_first = false;
                }
                Ok(any_diff)
            }
            _ => all_different(left, right),
        }
    }
}

impl Default for DisplayTree<'_> {
    fn default() -> Self {
        Self::leaf_noalloc("")
    }
}

impl<'a> Display for DisplayTree<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            Leaf(s) => write!(f, "{s}")?,
            Separated { sep, children, .. } => {
                let mut is_first = true;
                for child in children {
                    if !is_first {
                        write!(f, "{sep}")?;
                    }
                    write!(f, "{child}")?;
                    is_first = false;
                }
            }
        }
        Ok(())
    }
}

pub trait ToDisplayTree<'a> {
    fn to_display_tree(&self, a: &'a Arenas<'a>) -> DisplayTree<'a>;
}

impl<'a, T: ToDisplayTree<'a>> ToDisplayTree<'a> for &T {
    fn to_display_tree(&self, a: &'a Arenas<'a>) -> DisplayTree<'a> {
        (*self).to_display_tree(a)
    }
}
impl<'a> ToDisplayTree<'a> for &str {
    fn to_display_tree(&self, a: &'a Arenas<'a>) -> DisplayTree<'a> {
        DisplayTree::leaf(a, self)
    }
}
impl<'a> ToDisplayTree<'a> for String {
    fn to_display_tree(&self, a: &'a Arenas<'a>) -> DisplayTree<'a> {
        self.as_str().to_display_tree(a)
    }
}
impl<'a> ToDisplayTree<'a> for DisplayTree<'a> {
    fn to_display_tree(&self, _: &'a Arenas<'a>) -> DisplayTree<'a> {
        *self
    }
}
