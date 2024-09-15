use std::{cmp::Ordering, mem, ops::ControlFlow};

use crate::*;
use TypingResult::{BorrowError, Success};

pub type ParseError = String;

/// Holds the partial state of a comparison between two rulesets. We want to explore all patterns
/// and types up to a given depth. The idea is to iteratively deepen predicates so as to avoid
/// duplicate work. Given a partially-abstract predicate, we make progress with both rulesets until
/// they both require deepening. We then deepen, up to the set limit. If at any point one of the
/// rulesets errors, we stop deepening.
#[derive(Debug)]
struct ComparisonState<'a> {
    remaining_pat_depth: usize,
    remaining_ty_depth: usize,
    /// Tracks the starting request that corresponds to the current state.
    req: TypingRequest<'a>,
    /// Tracks where we got to starting from `req` and stepping with the left ruleset until we
    /// can't.
    left_state: ControlFlow<TypingResult<'a>, TypingPredicate<'a>>,
    /// Same with the right ruleset.
    right_state: ControlFlow<TypingResult<'a>, TypingPredicate<'a>>,
}

impl<'a> ComparisonState<'a> {
    fn deepen_pat(self, a: &'a Arenas<'a>) -> Vec<Self> {
        match self.remaining_pat_depth.checked_sub(1) {
            None => Vec::new(),
            Some(remaining) => DEPTH1_PATS
                .iter()
                // Discard abstract ones at the last stage.
                .filter(|pat| !(remaining == 0 && pat.contains_abstract()))
                .map(|&pat| ComparisonState {
                    remaining_pat_depth: remaining,
                    remaining_ty_depth: self.remaining_ty_depth,
                    req: self.req.subst_pat(a, pat),
                    left_state: self.left_state.map_continue(|pred| pred.subst_pat(a, pat)),
                    right_state: self.right_state.map_continue(|pred| pred.subst_pat(a, pat)),
                })
                .collect(),
        }
    }

    fn deepen_ty(self, a: &'a Arenas<'a>) -> Vec<Self> {
        match self.remaining_ty_depth.checked_sub(1) {
            None => Vec::new(),
            Some(remaining) => DEPTH1_TYS
                .iter()
                // Discard abstract ones at the last stage.
                .filter(|ty| !(remaining == 0 && ty.contains_abstract()))
                .map(|&ty| ComparisonState {
                    remaining_pat_depth: self.remaining_pat_depth,
                    remaining_ty_depth: remaining,
                    req: self.req.subst_ty(a, ty),
                    left_state: self
                        .left_state
                        .map_continue(|pred| pred.subst_ty(a, ty))
                        .map_break(|res| res.subst_ty(a, ty)),
                    right_state: self
                        .right_state
                        .map_continue(|pred| pred.subst_ty(a, ty))
                        .map_break(|res| res.subst_ty(a, ty)),
                })
                .collect(),
        }
    }

    /// Step the `Continue` case until completion or deepening.
    fn step_half(
        a: &'a Arenas<'a>,
        ruleset: RuleSet,
        current_req: &TypingRequest<'a>,
        state: &mut ControlFlow<TypingResult<'a>, TypingPredicate<'a>>,
    ) -> Option<DeepeningRequest> {
        use ControlFlow::*;
        while let Continue(pred) = state {
            *state = match ruleset {
                RuleSet::BindingModeBased(conf) => {
                    match analyze_with_formality(a, conf, current_req) {
                        TypingResult::TypeError(TypeError::OverlyGeneral(deepening)) => {
                            return Some(deepening)
                        }
                        res => Break(res),
                    }
                }
                RuleSet::TypeBased(options) => {
                    let ctx = TypingCtx { arenas: a, options };
                    match pred.step(ctx) {
                        Ok((_rule, next)) => {
                            if next.is_empty() {
                                let ty = *pred.expr.ty;
                                match pred.expr.simplify(ctx).borrow_check() {
                                    Err(BorrowCheckError::OverlyGeneral(deepening)) => {
                                        return Some(deepening)
                                    }
                                    Err(err) => Break(BorrowError(ty, err)),
                                    Ok(_) => Break(Success(ty)),
                                }
                            } else {
                                assert_eq!(next.len(), 1); // we only deepen with arity-1 tuples
                                Continue(next[0])
                            }
                        }
                        Err(err) => {
                            if let TypeError::OverlyGeneral(deepening) = err {
                                return Some(deepening);
                            } else {
                                Break(TypingResult::TypeError(err))
                            }
                        }
                    }
                }
            }
        }
        None
    }

    /// Step both states until completion or deepening.
    fn step(
        &mut self,
        a: &'a Arenas<'a>,
        left_ruleset: RuleSet,
        right_ruleset: RuleSet,
    ) -> Option<DeepeningRequest> {
        // Step the left state until completion or deepening.
        let left_deepening = Self::step_half(a, left_ruleset, &self.req, &mut self.left_state);
        // Step the right state until completion or deepening.
        let right_deepening = Self::step_half(a, right_ruleset, &self.req, &mut self.right_state);
        left_deepening.or(right_deepening)
    }
}

pub fn compare_rulesets<'a>(
    a: &'a Arenas<'a>,
    pat_depth: usize,
    ty_depth: usize,
    left_ruleset: RuleSet,
    expected_order: Ordering,
    right_ruleset: RuleSet,
) -> Vec<(TypingRequest<'a>, TypingResult<'a>, TypingResult<'a>)> {
    use ControlFlow::*;
    use Ordering::*;
    use TypingResult::*;

    // Start with an abstract pattern and type, and a concrete expression.
    let req = TypingRequest::ABSTRACT;
    let start_state = ControlFlow::Continue(TypingPredicate::new(req));

    let mut states = vec![ComparisonState {
        remaining_pat_depth: pat_depth + 1,
        remaining_ty_depth: ty_depth + 1,
        req,
        left_state: start_state,
        right_state: start_state,
    }];
    let mut complete = vec![];
    while !states.is_empty() {
        for mut state in mem::take(&mut states) {
            // Deepen each state until either completion or deepening is required.
            let opt_deepen = state.step(a, left_ruleset, right_ruleset);
            let left_res = state.left_state;
            let right_res = state.right_state;
            // No need to continue if one side already errored as expected.
            match expected_order {
                Less if matches!(left_res, Break(TypeError(_) | BorrowError(..))) => continue,
                Greater if matches!(right_res, Break(TypeError(_) | BorrowError(..))) => continue,
                _ => {}
            }
            match opt_deepen {
                None => {
                    // No deepening required, hence this is a concrete predicate or both errored.
                    let left_res = left_res.break_value().unwrap();
                    let right_res = right_res.break_value().unwrap();
                    // Skip if the results match.
                    match (left_res, right_res) {
                        (Success(lty), Success(rty)) if lty == rty => continue,
                        (TypeError(_) | BorrowError(..), TypeError(_) | BorrowError(..)) => {
                            continue
                        }
                        // These borrow errors only come from this solver, whose borrow checker is
                        // more accurate than the other. Hence if both agree on types we ignore the
                        // borrowck error.
                        (BorrowError(lty, _), Success(rty))
                            if right_ruleset.is_bm_based() && lty == rty =>
                        {
                            continue
                        }
                        (Success(lty), BorrowError(rty, _))
                            if left_ruleset.is_bm_based() && lty == rty =>
                        {
                            continue
                        }
                        _ => {}
                    }
                    complete.push((state.req, left_res, right_res))
                }
                Some(DeepeningRequest::Pattern) => states.extend(state.deepen_pat(a)),
                Some(DeepeningRequest::Type) => states.extend(state.deepen_ty(a)),
                Some(_) => unreachable!("the expression should be concrete"),
            }
        }
    }

    complete.sort_by_key(|(req, _, _)| (req.depth(), *req));
    complete
}

#[test]
/// Compare rulesets with the `ergo-formality` reference implementation.
fn compare() {
    use std::fmt::Write;
    use Ordering::*;
    use RuleSet::*;
    use TestKind::*;

    #[derive(Debug, Clone, Copy, PartialEq, Eq)]
    enum TestKind {
        /// The comparison must really hold.
        ForReal,
        /// The comparison doesn't hold, and we store a snapshot of the differences.
        Somewhat,
    }

    impl TestKind {
        fn expect(self, expected_order: Ordering) -> TestSettings {
            TestSettings {
                kind: self,
                expected_order,
                pat_depth: 3,
                ty_depth: 4,
            }
        }
    }

    #[derive(Clone, Copy)]
    struct TestSettings {
        kind: TestKind,
        expected_order: Ordering,
        pat_depth: usize,
        ty_depth: usize,
    }

    impl TestSettings {
        fn deeper(mut self) -> Self {
            self.pat_depth += 1;
            self.ty_depth += 1;
            self
        }
        fn shallower(mut self) -> Self {
            self.pat_depth -= 1;
            self.ty_depth -= 1;
            self
        }
    }

    let a = &Arenas::default();
    let compare: &[(&str, RuleSet, TestSettings, RuleSet)] = &[
        (
            "nadri",
            TypeBased(RuleOptions {
                // `ergo-formality` doesn't support the `Keep` option.
                mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
                // `ergo-formality` doesn't support any option.
                ref_binding_on_inherited: RefBindingOnInheritedBehavior::ResetBindingMode,
                ..RuleOptions::NADRI
            }),
            Somewhat.expect(Equal).shallower(),
            BindingModeBased({
                let mut c = Conf::default();
                c.rule1 = true;
                c.rule4_early = true;
                c.rule5 = true;
                c
            }),
        ),
        (
            "stable_rust",
            TypeBased(RuleOptions::STABLE_RUST),
            ForReal.expect(Equal),
            BindingModeBased(Conf::rfc2005()),
        ),
        (
            "structural",
            TypeBased(RuleOptions::STRUCTURAL),
            ForReal.expect(Equal),
            BindingModeBased(Conf::pre_rfc2005()),
        ),
        (
            "ergo2024",
            TypeBased(RuleOptions::ERGO2024),
            ForReal.expect(Equal),
            BindingModeBased(Conf::rfc3627_2024()),
        ),
        (
            "rfc3627_2021",
            TypeBased(RuleOptions::RFC3627_2021),
            ForReal.expect(Equal),
            BindingModeBased(Conf::rfc3627_2021()),
        ),
        (
            "ergo2024_breaking_only",
            TypeBased(RuleOptions::ERGO2024_BREAKING_ONLY),
            ForReal.expect(Equal),
            BindingModeBased(Conf::rfc_3627_2024_min()),
        ),
        (
            "waffle",
            TypeBased(RuleOptions::WAFFLE),
            ForReal.expect(Equal),
            BindingModeBased({
                let mut c = Conf::waffle_2024();
                // Supporting the rule3 extension is too complicated.
                c.rule3_ext1 = false;
                c.rule3 = true;
                c
            }),
        ),
        (
            "rpjohnst",
            TypeBased(RuleOptions::RPJOHNST),
            Somewhat.expect(Equal).shallower().shallower(),
            BindingModeBased(Conf::rpjohnst_2024()),
        ),
        (
            "ergo2024_nonbreaking_transition_bm_based",
            BindingModeBased(Conf::rfc_3627_2024_min()),
            Somewhat.expect(Less),
            BindingModeBased(Conf::rfc3627_2024()),
        ),
        (
            "ergo2024_nonbreaking_transition_type_based",
            TypeBased(RuleOptions::ERGO2024_BREAKING_ONLY),
            ForReal.expect(Less).deeper(),
            TypeBased(RuleOptions::ERGO2024),
        ),
        (
            "ergo2024_breaking_vs_stateless_with_rule3",
            TypeBased(RuleOptions {
                ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
                mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
                ..RuleOptions::ERGO2024_BREAKING_ONLY
            }),
            Somewhat.expect(Equal),
            TypeBased(RuleOptions {
                ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
                mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
                allow_ref_pat_on_ref_mut: false,
                eat_inherited_ref_alone: false,
                downgrade_mut_inside_shared: true,
                ..RuleOptions::STATELESS
            }),
        ),
        (
            "minimal_breaking_transition_to_stateless",
            TypeBased(RuleOptions::ERGO2024_BREAKING_ONLY_EXT),
            Somewhat.expect(Less).deeper().deeper(),
            TypeBased(RuleOptions::STATELESS),
        ),
        (
            "minimal_breaking_transition_to_stateless_with_rule3",
            TypeBased(RuleOptions::ERGO2024_BREAKING_ONLY_EXT),
            Somewhat.expect(Less).deeper().deeper(),
            TypeBased(RuleOptions {
                downgrade_mut_inside_shared: true,
                ..RuleOptions::STATELESS
            }),
        ),
        (
            "minimal_breaking_transition_to_rfc3627_breaking",
            TypeBased(RuleOptions::ERGO2024_BREAKING_ONLY_EXT),
            ForReal.expect(Less).deeper().deeper(),
            TypeBased(RuleOptions::ERGO2024_BREAKING_ONLY),
        ),
        (
            "minimal_breaking_transition_to_waffle",
            TypeBased(RuleOptions::ERGO2024_BREAKING_ONLY_EXT),
            Somewhat.expect(Less).deeper(),
            TypeBased(RuleOptions::WAFFLE),
        ),
        (
            "non_breaking_on_stable",
            TypeBased(RuleOptions::STABLE_RUST),
            ForReal.expect(Less),
            TypeBased(RuleOptions::RFC3627_2021),
        ),
        (
            "stable_vs_stateless",
            TypeBased(RuleOptions {
                ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
                mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
                ..RuleOptions::STABLE_RUST
            }),
            Somewhat.expect(Equal).shallower(),
            TypeBased(RuleOptions {
                ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
                mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
                allow_ref_pat_on_ref_mut: false,
                eat_inherited_ref_alone: false,
                ..RuleOptions::STATELESS
            }),
        ),
        (
            "rfc3627_vs_stateless",
            TypeBased(RuleOptions {
                ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
                mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
                allow_ref_pat_on_ref_mut: true,
                downgrade_mut_inside_shared: true,
                eat_mut_inside_shared: false,
                ..RuleOptions::ERGO2024
            }),
            Somewhat.expect(Equal).shallower(),
            TypeBased(RuleOptions {
                ref_binding_on_inherited: RefBindingOnInheritedBehavior::Error,
                mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
                allow_ref_pat_on_ref_mut: true,
                downgrade_mut_inside_shared: true,
                ..RuleOptions::STATELESS
            }),
        ),
        (
            "stateless_add_rule3",
            TypeBased(RuleOptions::STATELESS),
            Somewhat.expect(Less),
            TypeBased(RuleOptions {
                downgrade_mut_inside_shared: true,
                ..RuleOptions::STATELESS
            }),
        ),
    ];

    for &(name, left_ruleset, settings, right_ruleset) in compare {
        let differences = compare_rulesets(
            a,
            settings.pat_depth,
            settings.ty_depth,
            left_ruleset,
            settings.expected_order,
            right_ruleset,
        );

        if differences.is_empty() {
            assert_eq!(settings.kind, ForReal, "`{name}`: comparison did hold");
        } else {
            let mut trace = String::new();
            for (test_case, left_res, right_res) in differences {
                let test_case_str = test_case.to_string();
                let _ = writeln!(&mut trace, "Difference on `{test_case_str}`:");
                let _ = writeln!(&mut trace, "   left returned: {left_res}");
                let _ = writeln!(&mut trace, "  right returned: {right_res}");
            }

            insta::with_settings!({
                snapshot_path => "../../tests/snapshots",
                snapshot_suffix => format!("{name}"),
                prepend_module_to_snapshot => false,
                omit_expression => true,
                info => &name,
            }, {
                insta::assert_snapshot!(trace);
            });

            assert_eq!(settings.kind, Somewhat, "`{name}`: comparison did not hold");
        }
    }
}
