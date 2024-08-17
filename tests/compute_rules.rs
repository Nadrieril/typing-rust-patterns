use std::fmt::Write;

use typing_rust_patterns::*;

#[test]
fn compute_rules() -> anyhow::Result<()> {
    let arenas = &Arenas::default();
    for &(name, options, _) in RuleOptions::KNOWN_OPTION_BUNDLES {
        let ctx = TypingCtx { arenas, options };

        let mut typing_rules = typing_rust_patterns::compute_rules(ctx);
        typing_rules.sort_by_key(|rule| rule.name);

        let mut rules_str = String::new();
        for rule in typing_rules {
            let _ = writeln!(&mut rules_str, "{rule}\n");
        }

        insta::with_settings!({
            snapshot_suffix => name,
            info => &name,
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(rules_str);
        });
    }

    Ok(())
}
