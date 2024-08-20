use std::{collections::HashSet, fmt::Display, io::IsTerminal};

use anyhow::bail;
use colored::Color;
use inquire::{history::SimpleHistory, CustomUserError, Text};
use itertools::Itertools;
use typing_rust_patterns::*;

static COMMANDS: &[&str] = &["help", "options", "set", "rules", "quit"];

fn main() -> anyhow::Result<()> {
    let is_interactive = std::io::stdin().is_terminal();

    if is_interactive {
        println!("Welcome to the interactive pattern typer!");
        println!("Write `pattern: type` on the prompt line and I will attempt to type it.");
        println!("Example: `&[ref x]: &[T]`");
        println!(
        "Type `help` for a list of available commands. Type the command for usage instructions."
    );
        println!("");
    }

    let mut options = RuleOptions::DEFAULT;

    let mut history = Vec::new();
    let prompt = |history: &[_]| {
        if is_interactive {
            Text::new("")
                .with_placeholder("[&x]: &mut [&T]")
                .with_autocomplete(Autocomplete)
                .with_history(SimpleHistory::new(history.iter().rev().cloned().collect()))
                .prompt_skippable()
        } else {
            let mut buffer = String::new();
            std::io::stdin().read_line(&mut buffer)?;
            Ok(if buffer.is_empty() {
                None
            } else {
                Some(buffer)
            })
        }
    };
    while let Some(request) = prompt(&history)? {
        let request = request.trim();
        if request == "?" || request == "help" {
            println!("Commands: options, set, rules, quit");
        } else if request == "q" || request == "quit" {
            break;
        } else if request == "options" {
            let options = serde_yaml::to_string(&options)?;
            print!("{options}");
        } else if request == "rules" {
            display_rules(options)
        } else if let Some(cmd) = request.strip_prefix("set") {
            history.push(request.to_string());
            let old_options = options;
            match parse_set_cmd(cmd, &mut options) {
                // Display what changed.
                Ok(_) => {
                    display_options_diff(old_options, options);
                    println!();
                    display_rules_diff(old_options, options);
                }
                Err(err) => {
                    println!(
                        "Error: {err}\n\n\
                        Options are:\n\
                        {}\n\
                        There also exist some predefined option-bundles. Activate one with `set <bundle>`\n\
                        {}",
                        RuleOptions::OPTIONS_DOC.iter().map(|(name, values, descr)| format!("- {name}: {}\n    {descr}\n", values.iter().format(" | "))).format(""),
                        RuleOptions::KNOWN_OPTION_BUNDLES.iter().map(|(name, _, descr)| format!("- {name}: {descr}")).format("\n")
                    )
                }
            }
        } else {
            history.push(request.to_string());
            match trace_solver(&request, options) {
                Ok(trace) => println!("{trace}"),
                Err(err) => {
                    println!(
                        "Couldn't parse typing request ({err}).\n\n\
                        Syntax is `pattern: type`.\n\
                        Available patterns are:\n\
                        - bindings `x`, `ref y`, `mut z`, etc\n\
                        - references `&p`, `&mut p`\n\
                        - tuples `[p]`, `[p, q]`\n\
                        Available types are:\n\
                        - variables `T`, `U`, etc\n\
                        - references `&T`\n\
                        - tuples `[T]`, `[T, U]`"
                    )
                }
            }
        }
    }
    Ok(())
}

fn parse_set_cmd(cmd: &str, options: &mut RuleOptions) -> anyhow::Result<()> {
    let cmd = cmd.trim();
    if cmd == "" {
        bail!("Syntax is `set option value`.")
    }
    if let Some(opt) = RuleOptions::from_bundle_name(cmd) {
        *options = opt;
        return Ok(());
    }
    let cmd = cmd.split(" ").collect_vec();
    let ([opt, val] | [opt, "=", val]) = cmd.as_slice() else {
        bail!("couldn't parse `set` command.\nSyntax is `set option value`.")
    };
    options.set_key(opt, val)?;
    Ok(())
}

fn display_options_diff(old_options: RuleOptions, new_options: RuleOptions) {
    let old_options = old_options.to_map();
    let new_options = new_options.to_map();
    for (k, v) in &old_options {
        let new_v = &new_options[k];
        if v != new_v {
            println!("{k}: {v} -> {new_v}");
        }
    }
}

fn display_rules(options: RuleOptions) {
    println!("The current options can be fully described as the following set of rules.");

    let arenas = &Arenas::default();
    let ctx = TypingCtx { arenas, options };
    if options.downgrade_mut_inside_shared {
        println!("Warning: option `downgrade_mut_inside_shared` is not represented in the rules");
    }

    println!();

    let typing_rules = compute_rules(ctx);
    for rule in typing_rules {
        println!("{}\n", rule.display(options.rules_display_style));
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum DiffState {
    Both,
    Old,
    New,
}

impl DiffState {
    fn display<'a>(&self, text: &'a str) -> impl Display + 'a {
        use colored::Colorize;
        let (marker, color) = match self {
            Self::New => ("+", Some(Color::Green)),
            Self::Old => ("-", Some(Color::Red)),
            Self::Both => (" ", None),
        };
        text.lines()
            .map(move |line| {
                let line = format!("{marker}{line}");
                if let Some(color) = color {
                    line.color(color)
                } else {
                    <&str as Colorize>::clear(&line)
                }
            })
            .format("\n")
    }
}

fn display_rules_diff(old_options: RuleOptions, new_options: RuleOptions) {
    let arenas = &Arenas::default();
    let old_rules = compute_rules(TypingCtx {
        arenas,
        options: old_options,
    });
    let new_rules = compute_rules(TypingCtx {
        arenas,
        options: new_options,
    });

    let mut all_rules: Vec<(DiffState, &TypingRule)> = Vec::new();
    let new_rules_hashed: HashSet<&TypingRule> = new_rules.iter().collect();
    for rule in &old_rules {
        let state = if new_rules_hashed.contains(rule) {
            DiffState::Both
        } else {
            DiffState::Old
        };
        all_rules.push((state, rule));
    }
    let old_rules_hashed: HashSet<&TypingRule> = old_rules.iter().collect();
    for rule in &new_rules {
        if !old_rules_hashed.contains(rule) {
            all_rules.push((DiffState::New, rule));
        }
    }
    all_rules.sort_by_key(|(_, rule)| rule.name);

    if old_options.downgrade_mut_inside_shared || new_options.downgrade_mut_inside_shared {
        println!("Warning: option `downgrade_mut_inside_shared` is not represented in the rules");
        println!();
    }
    // Display the rules diff.
    for (state, rule) in all_rules {
        match state {
            DiffState::Both => {
                if old_options.rules_display_style != new_options.rules_display_style {
                    // Show the style diff if there is one.
                    let old_style = rule.display(old_options.rules_display_style).to_string();
                    let new_style = rule.display(new_options.rules_display_style).to_string();
                    if old_style == new_style {
                        continue;
                    }
                    println!("{}\n", DiffState::Old.display(&old_style));
                    println!("{}\n", DiffState::New.display(&new_style));
                }
            }
            DiffState::Old => {
                let rule_str = rule.display(old_options.rules_display_style).to_string();
                println!("{}\n", state.display(&rule_str));
            }
            DiffState::New => {
                let rule_str = rule.display(new_options.rules_display_style).to_string();
                println!("{}\n", state.display(&rule_str));
            }
        }
    }
}

#[derive(Clone)]
struct Autocomplete;

impl inquire::Autocomplete for Autocomplete {
    fn get_suggestions(&mut self, input: &str) -> Result<Vec<String>, CustomUserError> {
        let mut ret = vec![];
        if input.is_empty() {
            return Ok(ret);
        }
        let input = &input.to_lowercase();

        for &cmd in COMMANDS {
            if cmd.starts_with(input) && cmd != input {
                ret.push(format!("{cmd}"))
            }
        }

        if let Some(opt) = input.strip_prefix("set") {
            let opt = opt.trim();
            for &(name, values, _) in RuleOptions::OPTIONS_DOC {
                if let Some(val) = opt.strip_prefix(name) {
                    let val = val.trim();
                    for possible_value in values {
                        if possible_value.to_lowercase().starts_with(val) {
                            ret.push(format!("set {name} {possible_value}"));
                        }
                    }
                } else if name.starts_with(opt) {
                    ret.push(format!("set {name}"));
                }
            }
            for &(bundle, _, _) in RuleOptions::KNOWN_OPTION_BUNDLES {
                if bundle.starts_with(opt) {
                    ret.push(format!("set {bundle}"))
                }
            }
        }

        Ok(ret)
    }

    fn get_completion(
        &mut self,
        input: &str,
        suggestion: Option<String>,
    ) -> Result<Option<String>, CustomUserError> {
        if suggestion.is_some() {
            Ok(suggestion)
        } else {
            let suggestions: Vec<String> = self.get_suggestions(input)?;
            if suggestions.is_empty() {
                return Ok(None);
            }

            let mut longest_common_prefix = "";
            let len = suggestions.iter().map(|s| s.len()).min().unwrap();
            for i in 0..=len {
                if suggestions.iter().map(|s| &s[..i]).all_equal() {
                    longest_common_prefix = &suggestions[0][..i];
                } else {
                    break;
                }
            }
            if longest_common_prefix.is_empty() {
                Ok(None)
            } else {
                Ok(Some(longest_common_prefix.to_string()))
            }
        }
    }
}
