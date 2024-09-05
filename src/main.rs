use std::fmt::Write;
use std::{fmt::Display, io::IsTerminal};

use anyhow::bail;
use colored::Color;
use indoc::indoc;
use inquire::{history::SimpleHistory, CustomUserError, Text};
use itertools::Itertools;

use typing_rust_patterns::*;

static COMMANDS: &[(&str, &str)] = &[
    ("help", "view this help message"),
    ("set", "set options; type `set` for details"),
    ("options", "view the current value of each option"),
    (
        "rules",
        "display the typing rules implied by the current options",
    ),
    (
        "save",
        "save the current ruleset, for use with `swap` and `compare`",
    ),
    ("unsave", "forget the saved ruleset"),
    ("swap", "swap the current ruleset with the saved one"),
    (
        "compare",
        "compare the current ruleset against the saved one",
    ),
    ("quit", "quit"),
];

struct CliState {
    history: Vec<String>,
    options: RuleOptions,
    rules_display_style: TypingRuleStyle,
    saved: Option<RuleOptions>,
}

impl CliState {
    pub const CLI_OPTIONS: &[(&str, &[&str], &str)] = &[(
        "rules_display_style",
        &["Expression", "Sequent", "BindingMode", "Stateless"],
        "how to display typing rules (in the `rules` command)",
    )];

    fn prompt(&self, interactive: bool) -> anyhow::Result<Option<String>> {
        if interactive {
            Ok(Text::new("")
                .with_placeholder("[&x]: &mut [&T]")
                .with_autocomplete(Autocomplete)
                .with_history(SimpleHistory::new(
                    self.history.iter().rev().cloned().collect(),
                ))
                .prompt_skippable()?)
        } else {
            let mut buffer = String::new();
            std::io::stdin().read_line(&mut buffer)?;
            Ok(if buffer.is_empty() {
                None
            } else {
                Some(buffer)
            })
        }
    }

    fn settings() -> impl Iterator<Item = (&'static str, &'static [&'static str], &'static str)> {
        RuleOptions::OPTIONS_DOC
            .into_iter()
            .chain(Self::CLI_OPTIONS)
            .copied()
    }

    fn set_key(&mut self, key: &str, val: &str) -> anyhow::Result<()> {
        if let Some((_key, values, _doc)) = Self::settings().find(|(k, _, _)| *k == key) {
            if values.contains(&val) {
                if RuleOptions::OPTIONS_DOC
                    .iter()
                    .find(|(k, _, _)| *k == key)
                    .is_some()
                {
                    self.options.set_key(key, val)?;
                } else if key == "rules_display_style" {
                    self.rules_display_style = serde_yaml::from_str(val)?;
                } else {
                    bail!("oops, forgot to implement `set {key}`; please open an issue")
                }
                Ok(())
            } else {
                bail!(
                    "unknown value `{val}` for option `{key}`; options are: {}",
                    values.iter().format(", ")
                )
            }
        } else {
            bail!("unknown option `{key}`")
        }
    }

    fn display_joint_rules(
        &self,
        left: RuleOptions,
        right: RuleOptions,
    ) -> Result<String, IncompatibleStyle> {
        use DiffState::*;
        let style = self.rules_display_style;
        let arenas = &Arenas::default();
        let joint_rules = compute_joint_rules(arenas, left, right);

        let mut out = String::new();
        let _ = writeln!(&mut out, "The two rulesets are described by the following sets of rules, with differences highlighted.");
        let _ = writeln!(&mut out);
        for joint_rule in joint_rules {
            let (left, right) = joint_rule.left_and_right();
            let left = left
                .map(|r| r.display(style))
                .transpose()?
                .unwrap_or_default();
            let right = right
                .map(|r| r.display(style))
                .transpose()?
                .unwrap_or_default();
            for x in left.lines().zip_longest(right.lines()) {
                let (l, r) = x.or(" ", " ");
                let same = l == r;
                let l_state = if same { Both } else { Old };
                let r_state = if same { Both } else { New };
                let l = l_state.color_line(l);
                let r = r_state.color_line(r);
                let _ = writeln!(&mut out, " {l:80} | {r}");
            }
            let _ = writeln!(&mut out);
        }
        Ok(out)
    }
}

fn main() -> anyhow::Result<()> {
    let mut state = CliState {
        history: Vec::new(),
        options: RuleOptions::DEFAULT,
        rules_display_style: TypingRuleStyle::Expression,
        saved: None,
    };

    let is_interactive = std::io::stdin().is_terminal();
    if is_interactive {
        println!("Welcome to the interactive pattern typer!");
        println!("Write `pattern: type` on the prompt line and I will attempt to type it.");
        println!("Example: `&[ref x]: &[T]`");
        println!("Type `help` for a list of available commands.");
        println!("");
    }

    while let Some(request) = state.prompt(is_interactive)? {
        let request = request.trim();
        if request == "?" || request == "help" {
            println!(
                indoc!(
                    "
                Commands: {}

                {}
                "
                ),
                COMMANDS.iter().map(|(name, _)| name).format(", "),
                COMMANDS
                    .iter()
                    .map(|(name, doc)| format!("- {name}: {doc}"))
                    .format("\n"),
            )
        } else if request == "q" || request == "quit" {
            break;
        } else if request == "options" {
            let options = serde_yaml::to_string(&state.options)?;
            print!("{options}");
            let style = serde_yaml::to_string(&state.rules_display_style)?;
            print!("rules_display_style: {}", style);
        } else if request == "rules" {
            print!(
                "{}",
                display_rules(state.rules_display_style, state.options).unwrap()
            );
        } else if request == "save" {
            println!("Current ruleset was saved");
            state.saved = Some(state.options);
        } else if request == "unsave" {
            println!("Saved ruleset was forgotten");
            state.saved = None;
        } else if request == "swap" {
            if let Some(saved) = &mut state.saved {
                println!("Current and saved rulesets were swapped");
                std::mem::swap(saved, &mut state.options);
                let saved = *saved;
                let s = state.display_joint_rules(saved, state.options).unwrap();
                display_options_diff(saved, state.options);
                println!();
                print!("{s}")
            } else {
                println!("Can't swap saved and current ruleset because there is no saved ruleset. Use `save` to save one.");
            }
        } else if request == "compare" {
            if let Some(saved) = state.saved {
                if saved == state.options {
                    println!(indoc!(
                        "
                        This ruleset is the same as the one that was previously saved. Change some
                        settings and run `compare` again.
                        "
                    ))
                } else {
                    // TODO
                    // TODO: show sets of options at the top
                    let s = state.display_joint_rules(saved, state.options).unwrap();
                    print!("{s}");
                }
            } else {
                println!("Can't compare rulesets because there is no saved ruleset. Use `save` to save one.");
            }
        } else if let Some(cmd) = request.strip_prefix("set") {
            state.history.push(request.to_string());
            let old_style = state.rules_display_style;
            let old_options = state.options;
            match parse_set_cmd(cmd, &mut state) {
                // Display what changed.
                Ok(_) => {
                    if old_options != state.options {
                        match state.display_joint_rules(old_options, state.options) {
                            Ok(s) => {
                                display_options_diff(old_options, state.options);
                                println!();
                                print!("{s}")
                            }
                            Err(IncompatibleStyle) => {
                                state.options = old_options;
                                print!(
                                    "Error: the new ruleset cannot be displayed with style {}. ",
                                    state.rules_display_style
                                );
                                println!("Change the style and try again.");
                            }
                        }
                    } else {
                        if display_rules(state.rules_display_style, state.options).is_err() {
                            println!(
                                "Error: the current ruleset cannot be displayed with style {}.",
                                state.rules_display_style
                            );
                            state.rules_display_style = old_style;
                        } else if state.saved.is_some_and(|saved| {
                            display_rules(state.rules_display_style, saved).is_err()
                        }) {
                            println!(
                                "Error: the saved ruleset cannot be displayed with style {}.",
                                state.rules_display_style
                            );
                            state.rules_display_style = old_style;
                        }
                    }
                }
                Err(err) => {
                    println!(
                        "Error: {err}\n\n\
                        Options are:\n\
                        {}\n\
                        There also exist some predefined option-bundles. Activate one with `set <bundle>`\n\
                        {}",
                        CliState::settings().map(|(name, values, descr)| format!("- {name}: {}\n    {descr}\n", values.iter().format(" | "))).format(""),
                        RuleOptions::KNOWN_OPTION_BUNDLES.iter().map(|(name, _, descr)| format!("- {name}: {descr}")).format("\n")
                    )
                }
            }
        } else {
            // TODO: run in parallel if compare mode
            state.history.push(request.to_string());
            match trace_solver(&request, state.options, state.rules_display_style) {
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

fn parse_set_cmd(cmd: &str, state: &mut CliState) -> anyhow::Result<()> {
    let cmd = cmd.trim();
    if cmd == "" {
        bail!("Syntax is `set option value`.")
    }
    if let Some(opt) = RuleOptions::from_bundle_name(cmd) {
        state.options = opt;
        return Ok(());
    }
    let cmd = cmd.split(" ").collect_vec();
    let ([opt, val] | [opt, "=", val]) = cmd.as_slice() else {
        bail!("couldn't parse `set` command.\nSyntax is `set option value`.")
    };
    state.set_key(opt, val)?;
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

fn display_rules(
    style: TypingRuleStyle,
    options: RuleOptions,
) -> Result<String, IncompatibleStyle> {
    let arenas = &Arenas::default();
    let mut out = String::new();
    let _ = writeln!(
        &mut out,
        "The current options can be fully described as the following set of rules."
    );
    let _ = writeln!(
        &mut out,
        "The typing predicate looks like `{}`, where",
        TypingPredicate::new_abstract(arenas).display(style),
    );
    match style {
        TypingRuleStyle::Expression | TypingRuleStyle::BindingMode => {
            let _ = writeln!(&mut out, "- `e` is an expression",);
        }
        TypingRuleStyle::Sequent => {
            let _ = writeln!(
                &mut out,
                "- `b` is `place` or `value` and indicates the place context aka binding mode;",
            );
            let _ = writeln!(&mut out, "- `m` is `rw` or `ro` and indicates whether we have mutable or read-only access to the original scrutinee;",);
        }
        TypingRuleStyle::Stateless => {}
    }
    let _ = writeln!(&mut out, "- `p` is a pattern;");
    let _ = writeln!(&mut out, "- `T` is a type.");
    let _ = writeln!(&mut out);

    let ctx = TypingCtx { arenas, options };
    let typing_rules = compute_rules(ctx);
    for rule in typing_rules {
        let _ = writeln!(&mut out, "{}\n", rule.display(style)?);
    }
    Ok(out)
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum DiffState {
    Both,
    Old,
    New,
}

impl DiffState {
    // fn display<'a>(&self, text: &'a str) -> impl Display + 'a {
    //     use colored::Colorize;
    //     let (marker, color) = match self {
    //         Self::New => ("+", Some(Color::Green)),
    //         Self::Old => ("-", Some(Color::Red)),
    //         Self::Both => (" ", None),
    //     };
    //     text.lines()
    //         .map(move |line| {
    //             let line = format!("{marker}{line}");
    //             if let Some(color) = color {
    //                 line.color(color)
    //             } else {
    //                 <&str as Colorize>::clear(&line)
    //             }
    //         })
    //         .format("\n")
    // }

    fn color_line<'a>(&self, line: &'a str) -> impl Display + 'a {
        use colored::Colorize;
        let color = match self {
            Self::New => Some(Color::Green),
            Self::Old => Some(Color::Red),
            Self::Both => None,
        };
        if let Some(color) = color {
            line.color(color)
        } else {
            <&str as Colorize>::clear(line)
        }
    }
}

// fn display_rules_diff(old_options: RuleOptions, new_options: RuleOptions) {
//     let arenas = &Arenas::default();
//     let old_rules = compute_rules(TypingCtx {
//         arenas,
//         options: old_options,
//     });
//     let new_rules = compute_rules(TypingCtx {
//         arenas,
//         options: new_options,
//     });

//     let mut all_rules: Vec<(DiffState, &TypingRule)> = Vec::new();
//     let new_rules_hashed: HashSet<&TypingRule> = new_rules.iter().collect();
//     for rule in &old_rules {
//         let state = if new_rules_hashed.contains(rule) {
//             DiffState::Both
//         } else {
//             DiffState::Old
//         };
//         all_rules.push((state, rule));
//     }
//     let old_rules_hashed: HashSet<&TypingRule> = old_rules.iter().collect();
//     for rule in &new_rules {
//         if !old_rules_hashed.contains(rule) {
//             all_rules.push((DiffState::New, rule));
//         }
//     }
//     all_rules.sort_by_key(|(_, rule)| rule.name);

//     // Display the rules diff.
//     for (state, rule) in all_rules {
//         match state {
//             DiffState::Both => {
//                 if old_options.rules_display_style != new_options.rules_display_style {
//                     // Show the style diff if there is one.
//                     let old_style = rule.display(old_options.rules_display_style).to_string();
//                     let new_style = rule.display(new_options.rules_display_style).to_string();
//                     if old_style == new_style {
//                         continue;
//                     }
//                     println!("{}\n", DiffState::Old.display(&old_style));
//                     println!("{}\n", DiffState::New.display(&new_style));
//                 }
//             }
//             DiffState::Old => {
//                 let rule_str = rule.display(old_options.rules_display_style).to_string();
//                 println!("{}\n", state.display(&rule_str));
//             }
//             DiffState::New => {
//                 let rule_str = rule.display(new_options.rules_display_style).to_string();
//                 println!("{}\n", state.display(&rule_str));
//             }
//         }
//     }
// }

#[derive(Clone)]
struct Autocomplete;

impl inquire::Autocomplete for Autocomplete {
    fn get_suggestions(&mut self, input: &str) -> Result<Vec<String>, CustomUserError> {
        let mut ret = vec![];
        if input.is_empty() {
            return Ok(ret);
        }
        let input = &input.to_lowercase();

        for &(cmd, _) in COMMANDS {
            if cmd.starts_with(input) && cmd != input {
                ret.push(format!("{cmd}"))
            }
        }

        if let Some(opt) = input.strip_prefix("set") {
            let opt = opt.trim();
            for (name, values, _) in CliState::settings() {
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
