use std::fmt::Write;

use anyhow::Context;
use match_ergonomics_formality::Conf;
use typing_rust_patterns::*;

#[test]
fn compare() -> anyhow::Result<()> {
    let a = &Arenas::default();
    // TODO: test more equivalent rulesets.
    // - structural: no match ergonomics, no binding modes
    //     -> need an option to deactivate ConstructorRef
    // - find the right options that match my stateless and my proposal.
    let compare = [
        (
            "default",
            RuleOptions {
                // `ergo-formality` doesn't support the `Keep` option.
                mut_binding_on_inherited: MutBindingOnInheritedBehavior::Error,
                ..RuleOptions::DEFAULT
            },
            {
                let mut c = Conf::default();
                c.rule1 = true;
                c.rule4_early = true;
                c.rule5 = true;
                c
            },
        ),
        ("stable_rust", RuleOptions::STABLE_RUST, Conf::rfc2005()),
        ("ergo2024", RuleOptions::ERGO2024, Conf::rfc3627_2024()),
        (
            "rfc3627_2021",
            RuleOptions::RFC3627_2021,
            Conf::rfc3627_2021(),
        ),
        (
            "ergo2024_breaking_only",
            RuleOptions::ERGO2024_BREAKING_ONLY,
            Conf::rfc_3627_2024_min(),
        ),
        ("waffle", RuleOptions::WAFFLE, {
            let mut c = Conf::waffle_2024();
            c.rule3_ext1 = false; // Don't try to support this rule
            c
        }),
        ("rpjohnst", RuleOptions::RPJOHNST, Conf::rpjohnst_2024()),
    ];

    let test_cases = generate_requests(a, 3, 4);

    for (name, ty_based, bm_based) in compare {
        let ty_based = RuleSet::TypeBased(ty_based);
        let bm_based = RuleSet::BindingModeBased(bm_based);

        let mut trace = String::new();
        for test_case in &test_cases {
            let test_case_str = test_case.to_string();
            let left_res = &ty_based
                .analyze(a, *test_case)
                .context(test_case_str.clone())?;
            let right_res = &bm_based
                .analyze(a, *test_case)
                .context(test_case_str.clone())?;
            if left_res.matches(right_res) {
                continue;
            }
            let _ = writeln!(&mut trace, "Difference on `{test_case_str}`:");
            let _ = writeln!(&mut trace, "  type-based returned: {left_res:?}");
            let _ = writeln!(&mut trace, "    bm-based returned: {right_res:?}");
        }

        if !trace.is_empty() {
            insta::with_settings!({
                snapshot_suffix => format!("{name}"),
                info => &name,
                omit_expression => true,
                prepend_module_to_snapshot => true,
            }, {
                insta::assert_snapshot!(trace);
            });
        }
    }
    Ok(())
}
