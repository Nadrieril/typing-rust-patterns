use assert_cmd::Command;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

#[test]
fn trace_cli() -> anyhow::Result<()> {
    colored::control::set_override(false);
    let test_cases = &[
        "quit",
        "help",
        "options",
        "set",
        "set mistyped",
        "set mistyped val",
        "set ref_binding_on_inherited true",
        "set inherited_ref_on_ref EatInner",
        "set downgrade_mut_inside_shared true",
        "set rfc3627\noptions",
        "&x: &mut T",
        "&[[x]]: &[&mut [x]]",
        "set allow_ref_pat_on_ref_mut false\n&x: &mut T",
        "set structural\nrules",
        "set rfc3627\nset predicate_style SequentInMemory\nrules",
        "save\nset downgrade_mut_inside_shared true\nswap\ncompare",
        "save\nset structural\nswap\nrules",
        "set stable_rust\n[&x]: &[AT]",
        "&ap: &T",
        "set predicate_style SequentInMemory",
        "save;set structural;options",
        "save;set downgrade_mut_inside_shared true;swap;&[x]: &&mut [T]",
        // good to test trace diff
        "save;set rfc3627\n[&ref x, y]: &[&mut T, T]",
        // good to test rules diff
        "save;set fallback_to_outer EatOuter;set inherited_ref_on_ref EatInner",
        // hihi this crashes the cli
        // "save;set predicate_style SequentInMemory;swap;unsave;options;rules",
    ];
    for &test_case in test_cases {
        let success = Command::cargo_bin("typing-rust-patterns")?
            .write_stdin(test_case)
            .assert()
            .success();
        let err = String::from_utf8(success.get_output().stderr.clone())?;
        assert!(err.is_empty(), "nonempty stderr!:\n{err}");
        let out = String::from_utf8(success.get_output().stdout.clone())?;

        // Identify each snapshot file by the hash of the test case.
        let req_hash = {
            let mut hasher = DefaultHasher::new();
            test_case.hash(&mut hasher);
            hasher.finish().to_string()
        };
        insta::with_settings!({
            snapshot_suffix => req_hash,
            prepend_module_to_snapshot => false,
            omit_expression => true,
            info => &test_case,
        }, {
            insta::assert_snapshot!(out);
        });
    }
    Ok(())
}
