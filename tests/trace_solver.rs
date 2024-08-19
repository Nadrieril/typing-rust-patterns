use serde::Serialize;

use typing_rust_patterns::*;

#[derive(Hash, Serialize)]
struct TestCase<'a> {
    options: RuleOptions,
    bundle_name: Option<&'static str>,
    request: &'a str,
}

impl<'a> TestCase<'a> {
    fn new(request: &'a str, options: RuleOptions) -> Self {
        let bundle_name = options.get_bundle_name();
        Self {
            options,
            bundle_name,
            request,
        }
    }
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
    insta::with_settings!({
        snapshot_suffix => req_hash,
        info => &test_case,
        omit_expression => true,
        prepend_module_to_snapshot => true,
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
                "&[x]: &&mut [T]",
                "&[x]: &[&mut T]",
                "[&x]: &mut [&T]",
                // Borrow checking
                "[x]: [&mut T]",
                "&mut x: &mut &mut T",
                "&x: &&mut T",
                "&ref mut x: &mut T",
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
            RuleOptions::STATELESS,
            &[
                "&[[&x]]: &[&mut [T]]",
                "&[[&mut x]]: &[&mut [T]]",
                "[&ref mut x]: &mut [T]",
            ],
        ),
        (
            RuleOptions {
                ref_binding_on_inherited: RefBindingOnInheritedBehavior::ResetBindingMode,
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
                mut_binding_on_inherited: MutBindingOnInheritedBehavior::ResetBindingMode,
                ..RuleOptions::PERMISSIVE
            },
            &["mut x: &T", "[mut x]: &[T]", "[mut ref x]: &[T]"],
        ),
        (
            RuleOptions {
                eat_inherited_ref_alone: false,
                ..RuleOptions::PERMISSIVE
            },
            &["[&x]: &[[T]]", "[&x]: &mut [&T]", "[&mut x]: &mut [&T]"],
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
        (
            RuleOptions::ERGO2024,
            &[
                "&[x]: &&mut [T]",
                "&[x]: &[&mut T]",
                "[&x]: &mut [&T]",
                "[mut x]: &[T]",
                "[&mut x]: &[&mut T]",
                "[&mut &x]: &[&mut T]",
                "[&mut x]: &mut [&T]", // The tricky case of rule 4
                "[&mut ref mut x]: &mut [&T]",
            ],
        ),
        (
            RuleOptions::WAFFLE,
            &[
                "&[x]: &[&mut T]", // The tricky rule 3 variant
            ],
        ),
    ];

    for &(options, requests) in test_cases {
        for request in requests {
            let test_case = TestCase::new(request, options);
            spanshot_request(test_case)?;
        }
    }

    Ok(())
}
