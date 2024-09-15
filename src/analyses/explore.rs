use std::mem;

use itertools::Itertools;

use crate::*;
use BindingMode::*;
use Mutability::*;

mod deepen;
mod subst;

pub use deepen::*;

impl<'a> Pattern<'a> {
    pub fn depth(&self) -> usize {
        match self {
            Pattern::Abstract(_) => 0,
            Pattern::Binding(..) => 0,
            Pattern::Tuple(pats) => pats.iter().map(|pat| pat.depth() + 1).max().unwrap_or(0),
            Pattern::Ref(_, pat) => 1 + pat.depth(),
        }
    }

    /// Whether the pattern contains an abstract subpattern.
    pub fn contains_abstract(&self) -> bool {
        match *self {
            Pattern::Tuple(pats) => pats.iter().any(|pat| pat.contains_abstract()),
            Pattern::Ref(_, pat) => pat.contains_abstract(),
            Pattern::Binding(..) => false,
            Pattern::Abstract(_) => true,
        }
    }
}

impl<'a> Type<'a> {
    pub fn depth(&self) -> usize {
        match self {
            Type::Abstract(_) | Type::AbstractNonRef(_) | Type::OtherNonRef(_) => 0,
            Type::Tuple(tys) => tys.iter().map(|ty| ty.depth() + 1).max().unwrap_or(0),
            Type::Ref(_, ty) => 1 + ty.depth(),
        }
    }

    /// Whether the type contains an abstract subtype.
    pub fn contains_abstract(&self) -> bool {
        match *self {
            Type::Tuple(tys) => tys.iter().any(|ty| ty.contains_abstract()),
            Type::Ref(_, ty) => ty.contains_abstract(),
            Type::OtherNonRef(..) => false,
            Type::AbstractNonRef(..) | Type::Abstract(_) => true,
        }
    }
}

impl<'a> TypingRequest<'a> {
    pub fn depth(&self) -> usize {
        std::cmp::max(self.pat.depth(), self.ty.depth())
    }
}

//--- Generation ---

/// Patterns of depth 0 and 1. This is the same as `Pattern::ABSTRACT.deepen(_, false)`.
pub static DEPTH1_PATS: &[Pattern<'static>] = &[
    Pattern::Tuple(&[Pattern::ABSTRACT]),
    Pattern::Ref(Shared, &Pattern::ABSTRACT),
    Pattern::Ref(Mutable, &Pattern::ABSTRACT),
    Pattern::Binding(Shared, ByMove, "x"),
    Pattern::Binding(Mutable, ByMove, "x"),
    Pattern::Binding(Shared, ByRef(Shared), "x"),
    Pattern::Binding(Shared, ByRef(Mutable), "x"),
];

impl<'a> Pattern<'a> {
    /// Automatically generate concrete patterns up to a given depth.
    pub fn generate(a: &'a Arenas<'a>, depth: usize) -> Vec<&'a Self> {
        let mut out = Vec::new();
        // This contains patterns that have an abstract variable at depth exactly `n` if `n` is the
        // loop counter.
        let mut depth_ns = vec![&Pattern::ABSTRACT];
        for _ in 0..depth + 1 {
            for depthn in mem::take(&mut depth_ns) {
                for pat in &*DEPTH1_PATS {
                    let deeper = depthn.subst(a, *pat).alloc(a);
                    if pat.contains_abstract() {
                        // Keep the abstract patterns for next steps.
                        depth_ns.push(deeper);
                    } else {
                        // Output the concrete patterns.
                        out.push(deeper)
                    }
                }
            }
        }
        out
    }
}

/// Types of depth 0 and 1. This is the same as `Type::ABSTRACT.deepen(_, false)`.
pub static DEPTH1_TYS: &[Type<'_>] = &[
    Type::OtherNonRef("CT"),
    Type::Ref(Shared, &Type::ABSTRACT),
    Type::Ref(Mutable, &Type::ABSTRACT),
    Type::Tuple(&[Type::ABSTRACT]),
];

impl<'a> Type<'a> {
    /// Automatically generate concrete types up to a given depth.
    pub fn generate(a: &'a Arenas<'a>, depth: usize) -> Vec<&'a Self> {
        let mut out = Vec::new();
        // This contains types that have an abstract variable at depth exactly `n` if `n` is the
        // loop counter.
        let mut depth_ns = vec![&Type::ABSTRACT];
        for _ in 0..depth + 1 {
            for depthn in mem::take(&mut depth_ns) {
                for ty in &*DEPTH1_TYS {
                    let deeper = depthn.subst(a, *ty).alloc(a);
                    if ty.contains_abstract() {
                        // Keep the abstract types for next steps.
                        depth_ns.push(deeper);
                    } else {
                        // Output the concrete types.
                        out.push(deeper)
                    }
                }
            }
        }
        out
    }
}

impl<'a> TypingRequest<'a> {
    /// Automatically generate concrete requests up to a given depth.
    pub fn generate(a: &'a Arenas<'a>, pat_depth: usize, ty_depth: usize) -> Vec<Self> {
        let patterns = Pattern::generate(a, pat_depth);
        let types = Type::generate(a, ty_depth);
        let mut out = patterns
            .iter()
            .cartesian_product(types)
            .map(|(pat, ty)| TypingRequest { pat, ty })
            .collect_vec();
        out.sort_by_key(|req| (req.depth(), *req));
        out
    }
}
