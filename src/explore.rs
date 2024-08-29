use itertools::Itertools;

use crate::*;
use BindingMode::*;
use Mutability::*;

//--- Deepening ---

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum DeepeningRequest {
    Pattern,
    Type,
    BindingMode,
    ScrutineeMutability,
}

impl<'a> Pattern<'a> {
    /// Replace abstract subpatterns with all the possible more-precise patterns.
    pub fn deepen(&'a self, a: &'a Arenas<'a>, many: bool) -> Vec<Self> {
        match *self {
            Pattern::Abstract(name) => {
                let tuple = {
                    let subnames: &[&str] = if many {
                        // We assume no rules depend on the length. We use length 2 for demo
                        // purposes.
                        &[&(name.to_string() + "0"), &(name.to_string() + "1")]
                    } else {
                        &[name]
                    };
                    let subpats = subnames
                        .iter()
                        .map(|name| Pattern::Abstract(a.str_arena.alloc_str(name)));
                    Pattern::Tuple(a.pat_arena.alloc_extend(subpats))
                };
                [tuple]
                    .into_iter()
                    .chain(
                        Mutability::ALL
                            .into_iter()
                            .map(|mtbl| Pattern::Ref(mtbl, self)),
                    )
                    .chain(
                        Mutability::ALL
                            .into_iter()
                            .map(|mtbl| Pattern::Binding(mtbl, ByMove, "x")),
                    )
                    .chain(
                        Mutability::ALL
                            .into_iter()
                            .map(|mtbl| Pattern::Binding(Shared, ByRef(mtbl), "x")),
                    )
                    .collect()
            }
            Pattern::Tuple(pats) => {
                // Collect to avoid arena reentrancy problems.
                pats.iter()
                    .map(|p| p.deepen(a, many))
                    .multi_cartesian_product()
                    .map(|pats| Pattern::Tuple(a.pat_arena.alloc_extend(pats)))
                    .collect()
            }
            Pattern::Ref(mtbl, p) => p
                .deepen(a, many)
                .into_iter()
                .map(|p| Pattern::Ref(mtbl, p.alloc(a)))
                .collect(),
            Pattern::Binding(_, _, _) => vec![],
        }
    }
}

impl<'a> Type<'a> {
    /// Replace abstract subtypes with all the possible more-precise types.
    // TODO: explain what's up with `many`.
    pub fn deepen(&'a self, a: &'a Arenas<'a>, many: bool) -> Vec<Self> {
        let mk_tuple = |name: &str| {
            let subnames: &[&str] = if many {
                // We assume no rules depend on the length. We use length 2 for demo
                // purposes.
                &[&(name.to_string() + "0"), &(name.to_string() + "1")]
            } else {
                &[name]
            };
            let subtypes = subnames
                .iter()
                .map(|name| Type::Abstract(a.str_arena.alloc_str(&name)));
            Type::Tuple(a.type_arena.alloc_extend(subtypes))
        };

        match *self {
            Type::Abstract(name) => {
                vec![
                    if many {
                        Type::NonRef(name)
                    } else {
                        mk_tuple(name)
                    },
                    Type::Ref(Shared, self),
                    Type::Ref(Mutable, self),
                ]
            }
            Type::NonRef(name) => {
                if many {
                    vec![mk_tuple(name)]
                } else {
                    vec![]
                }
            }
            Type::Tuple(tys) => tys
                .iter()
                .map(|p| p.deepen(a, many))
                .multi_cartesian_product()
                .map(|tys| Type::Tuple(a.type_arena.alloc_extend(tys)))
                .collect(),
            Type::Ref(mtbl, p) => p
                .deepen(a, many)
                .into_iter()
                .map(|p| Type::Ref(mtbl, p.alloc(a)))
                .collect(),
        }
    }
}

impl<'a> Expression<'a> {
    /// Replace abstract subexpressions/subtypes with all the possible more-precise
    /// expressions/types.
    pub fn deepen(&self, a: &'a Arenas<'a>, req: DeepeningRequest, many: bool) -> Vec<Self> {
        use DeepeningRequest as D;
        match (self.kind, req) {
            (ExprKind::Scrutinee | ExprKind::Abstract { .. }, D::Type) => self
                .ty
                .deepen(a, many)
                .into_iter()
                .map(|ty| Expression {
                    kind: self.kind,
                    ty: ty.alloc(a),
                })
                .collect(),
            (ExprKind::Scrutinee, D::BindingMode) => vec![*self],
            (
                ExprKind::Abstract {
                    not_a_ref: false,
                    scrutinee_mutability,
                },
                D::BindingMode,
            ) => {
                // We know our rules only inspect the binding modes of expressions, so we only need
                // to split along that dimension.
                let mut vec = vec![Expression {
                    // Stands for any non-`Ref` expression.
                    kind: ExprKind::Abstract {
                        not_a_ref: true,
                        scrutinee_mutability,
                    },
                    ty: self.ty,
                }];
                // Add more `Ref` expressions, following the type if relevant.
                match *self.ty {
                    Type::Ref(mtbl, ty) => {
                        vec.push(
                            Expression {
                                kind: self.kind,
                                ty,
                            }
                            .borrow(a, mtbl),
                        );
                    }
                    Type::Abstract(..) => {
                        vec.push(self.borrow(a, Shared));
                        vec.push(self.borrow(a, Mutable));
                    }
                    Type::Tuple(..) | Type::NonRef(..) => {}
                }
                vec
            }
            (
                ExprKind::Abstract {
                    not_a_ref: true, ..
                },
                D::BindingMode,
            ) => {
                // This would generate Scrutinee/Deref/Field cases but we don't need it.
                unreachable!("A rule is inspecting expressions in unexpected ways")
            }
            (
                ExprKind::Abstract {
                    not_a_ref,
                    scrutinee_mutability,
                },
                D::ScrutineeMutability,
            ) => {
                assert!(scrutinee_mutability.is_none());
                Mutability::ALL
                    .into_iter()
                    .map(|mtbl| Expression {
                        kind: ExprKind::Abstract {
                            not_a_ref,
                            scrutinee_mutability: Some(mtbl),
                        },
                        ty: self.ty,
                    })
                    .collect()
            }
            (ExprKind::Scrutinee | ExprKind::Abstract { .. }, D::Pattern)
            | (ExprKind::Scrutinee, D::ScrutineeMutability) => unreachable!(),
            (ExprKind::Ref(mtbl, e), _) => e
                .deepen(a, req, many)
                .into_iter()
                .map(|e| e.borrow(a, mtbl))
                .collect(),
            (ExprKind::Deref(e), _) => e
                .deepen(a, req, many)
                .into_iter()
                .map(|e| e.deref(a))
                .collect(),
            (ExprKind::Field(e, n), _) => e
                .deepen(a, req, many)
                .into_iter()
                .map(|e| e.field(a, n))
                .collect(),
        }
    }
}

impl<'a> TypingPredicate<'a> {
    pub fn deepen_pat(self, a: &'a Arenas<'a>, many: bool) -> Vec<Self> {
        self.pat
            .deepen(a, many)
            .into_iter()
            .map(|pat| pat.alloc(a))
            .map(|pat| Self { pat, ..self })
            .collect()
    }
    pub fn deepen_expr(self, a: &'a Arenas<'a>, req: DeepeningRequest, many: bool) -> Vec<Self> {
        self.expr
            .deepen(a, req, many)
            .into_iter()
            .map(|expr| Self { expr, ..self })
            .collect()
    }

    pub fn deepen(self, a: &'a Arenas<'a>, req: DeepeningRequest, many: bool) -> Vec<Self> {
        match req {
            DeepeningRequest::Pattern => self.deepen_pat(a, many),
            _ => self.deepen_expr(a, req, many),
        }
    }
}

//--- Generation ---

impl<'a> Pattern<'a> {
    /// Automatically generate concrete patterns up to a given depth.
    pub fn generate(a: &'a Arenas<'a>, depth: usize) -> Vec<&'a Self> {
        pub fn generate<'a>(a: &'a Arenas<'a>, depth: usize) -> Vec<&'a Pattern<'a>> {
            if depth == 0 {
                return vec![&Pattern::Abstract("p")];
            }
            generate(a, depth - 1)
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
        generate(a, depth)
            .into_iter()
            .map(|pat| pat.subst(a, base_pat))
            .map(|pat| pat.alloc(a))
            .collect()
    }
}

impl<'a> Type<'a> {
    /// Automatically generate concrete types up to a given depth.
    pub fn generate(a: &'a Arenas<'a>, depth: usize) -> Vec<&'a Self> {
        pub fn generate<'a>(a: &'a Arenas<'a>, depth: usize) -> Vec<&'a Type<'a>> {
            if depth == 0 {
                return vec![&Type::Abstract("T")];
            }
            let base_ty = Type::NonRef("T");
            generate(a, depth - 1)
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
        generate(a, depth)
            .into_iter()
            .map(|ty| ty.subst(a, base_ty))
            .map(|ty| ty.alloc(a))
            .collect()
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
        out.sort_by_key(|req| req.depth());
        out
    }
}
