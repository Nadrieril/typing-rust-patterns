use typing_rust_patterns::*;

#[test]
fn test_solver_traces() {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    let requests: &[&str] = &[
        "&x: &T",
        "&x: T",
        "ref x: T",
        "&x: &T",
        "ref x: T",
        "ref mut x: T",
        "mut ref mut x: T",
        "[x]: &[T]",
        "[x, y]: &[T, U]",
        "[x, &y]: &[T, U]",
        "[ref x]: &[T]",
        "[&x]: &[&T]",
        "[&x]: &[&mut T]",
        "[&&mut x]: &[&mut T]",
        "[&mut x]: &mut [&T]",
        "[&mut x]: &[&mut T]",
        "&[[x]]: &[&mut [T]]",
        "&[[&x]]: &[&mut [T]]",
        "&[[&mut x]]: &[&mut [T]]",
        "[&ref mut x]: &mut [T]",
    ];
    for request in requests {
        let options = RuleOptions {
            ref_on_expr: RefOnExprBehavior::AllocTemporary,
            allow_ref_pat_on_ref_mut: true,
            simplify_expressions: true,
        };
        let trace = trace_solver(request, options);
        let req_hash = {
            let mut hasher = DefaultHasher::new();
            request.hash(&mut hasher);
            hasher.finish().to_string()
        };
        insta::with_settings!({
            snapshot_suffix => req_hash,
            omit_expression => true,
            prepend_module_to_snapshot => false,
        }, {
            insta::assert_snapshot!(trace);
        });
    }
}
