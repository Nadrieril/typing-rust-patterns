use std::collections::HashMap;
use std::fmt::Write;

use anyhow::Context;
use match_ergonomics_formality::Conf;
use typing_rust_patterns::*;

#[test]
fn compare() -> anyhow::Result<()> {
    let bm_based_map: HashMap<String, RuleSet> = [("ergo2024", Conf::rust_2024_proposed())]
        .into_iter()
        .map(|(name, conf)| (name.to_string(), RuleSet::BindingModeBased(conf)))
        .collect();

    // TODO: implement more equivalent rulesets.
    let compare = [("ergo2024", "ergo2024")];

    // TODO: explore automatically up to a given depth.
    let test_cases = &[
        "x: T",
        "x: &T",
        "x: &mut T",
        "&x: &T",
        "&x: T",
        "[x]: &&&[T]",
        "[x]: &&mut [T]",
        "[x]: &mut &[T]",
        "[x]: &mut &mut [T]",
        "ref x: T",
        "ref mut x: T",
        "&ref x: &T",
        "&ref mut x: &T",
        "&mut ref x: &mut T",
        "&mut ref mut x: &mut T",
        "[&mut ref x]: [&mut T]",
        "[x]: &[T]",
        "[ref x]: &[T]",
        "[ref mut x]: &[T]",
        "[ref x]: &mut [T]",
        "[ref mut x]: &mut [T]",
        "[&x]: &[&T]",
        "[&x]: &[&mut T]",
        "[&&mut x]: &[&mut T]",
        "[&mut x]: &mut [&T]", // The tricky case of rule 4
        "[&mut x]: &[&mut T]",
        "&[[x]]: &[&mut [T]]",
        "&[[&x]]: &[&mut [T]]",
        "&[[&mut x]]: &[&mut [T]]",
        "[&ref mut x]: &mut [T]",
        "&[x]: &&mut [T]",
        "&[x]: &[&mut T]",
        "[&x]: &mut [&T]",
        "[x]: [&mut T]",
        "&mut x: &mut &mut T",
        "&x: &&mut T",
        "&ref mut x: &mut T",
        "&[x]: &&mut [T]",
        "&[x]: &[&mut T]",
        "[&x]: &mut [&T]",
        "[mut x]: &[T]",
        "[&mut x]: &[&mut T]",
        "[&mut &x]: &[&mut T]",
    ];

    for (ty_based_name, bm_based_name) in compare {
        let ty_based = RuleSet::TypeBased(RuleOptions::from_bundle_name(ty_based_name).unwrap());
        let bm_based = bm_based_map.get(bm_based_name).unwrap();

        let a = &Arenas::default();
        let mut trace = String::new();
        for test_case in test_cases {
            let req = TypingRequest::parse(a, test_case).unwrap();
            let left_res = &ty_based.analyze(a, req).context(test_case)?;
            let right_res = &bm_based.analyze(a, req).context(test_case)?;
            if left_res.matches(right_res) {
                continue;
            }
            let _ = writeln!(&mut trace, "Difference on `{test_case}`:");
            let _ = writeln!(&mut trace, "  type-based returned: {left_res:?}");
            let _ = writeln!(&mut trace, "    bm-based returned: {right_res:?}");
        }

        insta::with_settings!({
            snapshot_suffix => format!("{ty_based_name}-{bm_based_name}"),
            info => &(ty_based_name, bm_based_name),
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(trace);
        });
    }
    Ok(())
}
