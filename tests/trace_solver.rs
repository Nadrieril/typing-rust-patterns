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
    // The most permissive options.
    let options = RuleOptions::PERMISSIVE;
    let requests: &[&str] = &[
        "&x: &T",
        "&x: T",
        "[x]: &&&[T]",
        "[x]: &&mut [T]",
        "[x]: &mut &[T]",
        "[x]: &mut &mut [T]",
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
        allow_ref_pat_on_ref_mut: false,
        ..RuleOptions::PERMISSIVE
    };
    let requests: &[&str] = &["&x: &mut T", "&[[&x]]: &[&mut [T]]"];
    for request in requests {
        spanshot_request(request, options)?;
    }

    let options = RuleOptions {
        simplify_expressions: false,
        ..RuleOptions::PERMISSIVE
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
        ref_on_ref: RefOnRefBehavior::Skip,
        ..RuleOptions::PERMISSIVE
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

    let options = RuleOptions {
        mut_on_ref: MutOnRefBehavior::ResetBindingMode,
        ..RuleOptions::PERMISSIVE
    };
    let requests: &[&str] = &["mut x: &T", "[mut x]: &[T]", "[mut ref x]: &[T]"];
    for request in requests {
        spanshot_request(request, options)?;
    }

    Ok(())
}
