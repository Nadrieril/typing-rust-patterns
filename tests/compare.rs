use std::collections::HashMap;
use std::fmt::Write;

use anyhow::Context;
use itertools::Itertools;
use match_ergonomics_formality::Conf;
use typing_rust_patterns::*;

#[test]
fn compare() -> anyhow::Result<()> {
    let a = &Arenas::default();
    let bm_based_map: HashMap<String, RuleSet> = [
        ("stable_rust", Conf::rfc2005()),
        ("ergo2024", Conf::rfc3627_2024()),
        ("waffle", {
            let mut c = Conf::waffle_2024();
            c.rule3_ext1 = false; // Don't try to support this rule
            c
        }),
    ]
    .into_iter()
    .map(|(name, conf)| (name.to_string(), RuleSet::BindingModeBased(conf)))
    .collect();

    // TODO: implement more equivalent rulesets.
    let compare = ["ergo2024", "stable_rust", "waffle"];

    let test_cases = {
        let patterns = generate_patterns(a, 2);
        let types = generate_types(a, 3);
        patterns
            .iter()
            .cartesian_product(types)
            .map(|(pat, ty)| TypingRequest { pat, ty })
            .collect_vec()
    };

    for name in compare {
        let ty_based = RuleSet::TypeBased(RuleOptions::from_bundle_name(name).unwrap());
        let bm_based = bm_based_map.get(name).unwrap();

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

        insta::with_settings!({
            snapshot_suffix => format!("{name}"),
            info => &name,
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(trace);
        });
    }
    Ok(())
}
