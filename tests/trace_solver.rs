use serde::Serialize;

use typing_rust_patterns::*;

#[derive(Hash, Serialize)]
struct TestCase<'a> {
    options: RuleOptions,
    request: &'a str,
}

fn spanshot_request(test_case: TestCase<'_>) -> anyhow::Result<()> {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    // Identify each snapshot file by the hash of the request and options.
    let req_hash = {
        let mut hasher = DefaultHasher::new();
        test_case.hash(&mut hasher);
        hasher.finish().to_string()
    };
    let trace = trace_solver(test_case.request, test_case.options)?;
    // let trace = format!("Query: `{request}`\n\n{trace}");
    insta::with_settings!({
        snapshot_suffix => req_hash,
        info => &test_case,
        omit_expression => true,
        prepend_module_to_snapshot => false,
    }, {
        insta::assert_snapshot!(trace);
    });
    Ok(())
}

#[test]
fn test_solver_traces() -> anyhow::Result<()> {
    let test_cases: &[(RuleOptions, &[_])] = &[
        (
            RuleOptions::PERMISSIVE,
            &[
                "&x: &T",
                "&x: T",
                "[x]: &&&[T]",
                "[x]: &&mut [T]",
                "[x]: &mut &[T]",
                "[x]: &mut &mut [T]",
                "ref x: T",
                "ref mut x: T",
                "mut ref mut x: T",
                "&ref x: &T",
                "&ref mut x: &T",
                "&mut ref x: &mut T",
                "&mut ref mut x: &mut T",
                "[&mut ref x]: [&mut T]",
                "[x]: &[T]",
                "[x, y]: &[T, U]",
                "[x, &y]: &[T, U]",
                "[ref x]: &[T]",
                "[ref mut x]: &[T]",
                "[ref x]: &mut [T]",
                "[ref mut x]: &mut [T]",
                "[&x]: &[&T]",
                "[&x]: &[&mut T]",
                "[&&mut x]: &[&mut T]",
                "[&mut x]: &mut [&T]",
                "[&mut x]: &[&mut T]",
                "&[[x]]: &[&mut [T]]",
                "&[[&x]]: &[&mut [T]]",
                "&[[&mut x]]: &[&mut [T]]",
                "[&ref mut x]: &mut [T]",
            ],
        ),
        (
            RuleOptions {
                allow_ref_pat_on_ref_mut: false,
                ..RuleOptions::PERMISSIVE
            },
            &["&x: &mut T", "&[[&x]]: &[&mut [T]]"],
        ),
        (
            RuleOptions {
                simplify_expressions: false,
                ..RuleOptions::PERMISSIVE
            },
            &[
                "&[[&x]]: &[&mut [T]]",
                "&[[&mut x]]: &[&mut [T]]",
                "[&ref mut x]: &mut [T]",
            ],
        ),
        (
            RuleOptions {
                ref_on_ref: RefOnRefBehavior::Skip,
                ..RuleOptions::PERMISSIVE
            },
            &[
                "[ref x]: &[T]",
                "[ref mut x]: &[T]",
                "[ref x]: &mut [T]",
                "[ref mut x]: &mut [T]",
            ],
        ),
        (
            RuleOptions {
                mut_on_ref: MutOnRefBehavior::ResetBindingMode,
                ..RuleOptions::PERMISSIVE
            },
            &["mut x: &T", "[mut x]: &[T]", "[mut ref x]: &[T]"],
        ),
        (
            RuleOptions::STABLE_RUST,
            &[
                "[&x]: &[T]",
                "[&x]: &[&T]",
                "[&x]: &[&&T]",
                "[&x]: &&[&&T]",
                "&[&x]: &&[&&T]",
            ],
        ),
    ];

    for &(options, requests) in test_cases {
        for request in requests {
            let test_case = TestCase { request, options };
            spanshot_request(test_case)?;
        }
    }

    Ok(())
}
