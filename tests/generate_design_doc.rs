use std::fmt::Write;

use typing_rust_patterns::*;

#[test]
fn generate_design_doc() -> anyhow::Result<()> {
    let options = RuleOptions::DEFAULT;

    let examples = [
        ("[&x]: &[&T]", "`x: &T`"),
        ("[&x]: &[&mut T]", "`x: &mut T`, move error"),
        ("[&&mut x]: &[&mut T]", "`x: T`"),
        ("[&mut x]: &mut [&T]", "`x: &T`"),
        ("[&mut x]: &[&mut T]", "type error"),
        ("&[[x]]: &[&mut [T]]", "`x: &mut T`, borrow error"),
        (
            "&[[&x]]: &[&mut [T]]",
            "`x: T`, borrow error if we don't use simplification rules",
        ),
        (
            "&[[&mut x]]: &[&mut [T]]",
            "`x: T`, borrow error if we don't use simplification rules",
        ),
        ("[&ref mut x]: &mut [T]", "`x: &mut T`, borrow error"),
    ];

    let mut doc = String::new();
    for (example, comment) in examples {
        let trace = trace_solver(example, options)?;
        writeln!(&mut doc, "- `{example}` => {comment}")?;
        writeln!(&mut doc, "```rust")?;
        writeln!(&mut doc, "{trace}```")?;
        writeln!(&mut doc)?;
    }

    insta::with_settings!({
        snapshot_suffix => "",
        info => &(),
        omit_expression => true,
        prepend_module_to_snapshot => false,
    }, {
        insta::assert_snapshot!(doc);
    });
    Ok(())
}
