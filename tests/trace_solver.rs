use typing_rust_patterns::*;

fn spanshot_request(request: &str, options: RuleOptions) -> anyhow::Result<()> {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    // Identify each snapshot file by the hash of the request and options.
    let req_hash = {
        let mut hasher = DefaultHasher::new();
        request.hash(&mut hasher);
        options.hash(&mut hasher);
        hasher.finish().to_string()
    };
    let trace = trace_solver(request, options)?;
    let trace = format!("Query: `{request}`\n\n{trace}");
    insta::with_settings!({
        snapshot_suffix => req_hash,
        description => format!("{options:?}"),
        omit_expression => true,
        prepend_module_to_snapshot => false,
    }, {
        insta::assert_snapshot!(trace);
    });
    Ok(())
}

#[test]
fn test_solver_traces() -> anyhow::Result<()> {
    let options = RuleOptions {
        ref_on_expr: RefOnExprBehavior::AllocTemporary,
        allow_ref_pat_on_ref_mut: true,
        simplify_expressions: true,
    };
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
    ];
    for request in requests {
        spanshot_request(request, options)?;
    }

    let options = RuleOptions {
        ref_on_expr: RefOnExprBehavior::AllocTemporary,
        allow_ref_pat_on_ref_mut: false,
        simplify_expressions: true,
    };
    let requests: &[&str] = &["&x: &mut T", "&[[&x]]: &[&mut [T]]"];
    for request in requests {
        spanshot_request(request, options)?;
    }

    let options = RuleOptions {
        ref_on_expr: RefOnExprBehavior::AllocTemporary,
        allow_ref_pat_on_ref_mut: true,
        simplify_expressions: false,
    };
    let requests: &[&str] = &[
        "&[[&x]]: &[&mut [T]]",
        "&[[&mut x]]: &[&mut [T]]",
        "[&ref mut x]: &mut [T]",
    ];
    for request in requests {
        spanshot_request(request, options)?;
    }

    let options = RuleOptions {
        ref_on_expr: RefOnExprBehavior::ResetBindingMode,
        allow_ref_pat_on_ref_mut: true,
        simplify_expressions: true,
    };
    let requests: &[&str] = &[
        "[ref x]: &[T]",
        "[ref mut x]: &[T]",
        "[ref x]: &mut [T]",
        "[ref mut x]: &mut [T]",
    ];
    for request in requests {
        spanshot_request(request, options)?;
    }

    Ok(())
}
