use assert_cmd::Command;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

#[test]
fn trace_cli() -> anyhow::Result<()> {
    let test_cases = &[
        "quit",
        "help",
        "options",
        "set",
        "set mistyped",
        "set mistyped val",
        "set ref_binding_on_inherited true",
        "set allow_ref_pat_on_ref_mut false",
        "set downgrade_shared_inside_shared true",
        "set ergo2024\noptions",
        "&x: &mut T",
        "set allow_ref_pat_on_ref_mut false\n&x: &mut T",
        "rules",
    ];
    for &test_case in test_cases {
        let success = Command::cargo_bin("typing-rust-patterns")?
            .write_stdin(test_case)
            .assert()
            .success();
        let out = String::from_utf8(success.get_output().stdout.clone())?;

        // Identify each snapshot file by the hash of the test case.
        let req_hash = {
            let mut hasher = DefaultHasher::new();
            test_case.hash(&mut hasher);
            hasher.finish().to_string()
        };
        insta::with_settings!({
            snapshot_suffix => req_hash,
            info => &test_case,
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(out);
        });
    }
    Ok(())
}
