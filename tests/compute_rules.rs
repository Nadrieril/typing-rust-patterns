use itertools::Itertools;
use serde::Serialize;
use std::fmt::Write;
use typing_rust_patterns::*;

#[derive(Serialize)]
struct TestCase {
    bundle_name: &'static str,
    options: RuleOptions,
}

#[test]
fn compute_rules() -> anyhow::Result<()> {
    let arenas = &Arenas::default();

    // Try both styles
    let bundles = RuleOptions::KNOWN_OPTION_BUNDLES
        .iter()
        .cartesian_product([TypingRuleStyle::Plain, TypingRuleStyle::BindingMode])
        .map(|(&(name, options, _), style)| {
            let options = RuleOptions {
                rules_display_style: style,
                ..options
            };
            (name, options)
        });

    for (name, options) in bundles {
        let ctx = TypingCtx { arenas, options };

        let mut typing_rules = typing_rust_patterns::compute_rules(ctx);
        typing_rules.sort_by_key(|rule| rule.name);

        let mut rules_str = String::new();
        for rule in typing_rules {
            let _ = writeln!(
                &mut rules_str,
                "{}\n",
                rule.display(options.rules_display_style)
            );
        }

        let info = TestCase {
            bundle_name: name,
            options,
        };
        insta::with_settings!({
            snapshot_suffix => format!("{name}-{:?}", options.rules_display_style),
            info => &info,
            omit_expression => true,
            prepend_module_to_snapshot => true,
        }, {
            insta::assert_snapshot!(rules_str);
        });
    }

    Ok(())
}
