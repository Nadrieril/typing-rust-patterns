use inquire::{history::SimpleHistory, Text};
use serde::Deserialize;

use itertools::Itertools;
use typing_rust_patterns::*;

fn main() -> anyhow::Result<()> {
    println!("Welcome to the interactive pattern typer!");
    println!("Write `pattern: type` on the prompt line and I will attempt to type it.");
    println!("Example: `&[ref x]: &[T]`");
    println!(
        "Type `help` for a list of available commands. Type the command for usage instructions."
    );
    println!("");

    let mut options = RuleOptions::NADRIS_PROPOSAL;

    let mut history = Vec::new();
    let prompt = |history: &[_]| {
        Text::new("")
            .with_history(SimpleHistory::new(history.iter().rev().cloned().collect()))
            .prompt_skippable()
    };
    while let Some(request) = prompt(&history)? {
        if request == "?" || request == "help" {
            println!("Commands: options, set, quit");
        } else if request == "q" || request == "quit" {
            break;
        } else if request == "options" || request == "option" {
            let options = serde_yaml::to_string(&options)?;
            print!("{options}");
        } else if let Some(cmd) = request.strip_prefix("set") {
            let old_options = options;
            if parse_set_cmd(cmd, &mut options).is_none() {
                println!(
                    "Couldn't parse `set` command.\n\n\
                    Syntax is `set option value`.\n\
                    Options are:\n\
                    - ref_on_ref: AllocTemporary | Skip | Error\n    \
                        how to handle a `ref x` binding on an inherited reference\n\
                    - mut_on_ref: ResetBindingMode | Keep | Error\n    \
                        how to handle a `mut x` binding on an inherited reference\n\
                    - allow_ref_pat_on_ref_mut: bool\n    \
                        whether to allow `&p: &mut T`\n\
                    - simplify_expressions: bool\n    \
                        whether to simplify some expressions, which removes some borrow errors\n\
                    - eat_two_layers: bool\n    \
                        whether `&p: &&T` eats both references when the outer one is inherited\n\
                    - eat_inherited_ref_alone: bool\n    \
                        whether `&p: &T` is allowed if the reference is inherited and `T` isn't some `&U`\n\
                    \n\
                    There also exist some predefined option-bundles. Activate one with `set bundle`\n\
                    - default: the default settings\n\
                    - permissive: an even more permissive proposal than the default\n\
                    - stateless: a proposal that tracks no hidden state; purely type-based\n\
                    - stable_rust: emulates the behavior of current stable rust"
                )
            } else {
                // Use yaml to show a nice diff of the options.
                let old_options = serde_yaml::to_string(&old_options)?;
                let new_options = serde_yaml::to_string(&options)?;
                let diff = similar::TextDiff::from_lines(&old_options, &new_options);
                for hunk in diff.unified_diff().context_radius(0).iter_hunks() {
                    for change in hunk.iter_changes() {
                        print!("{}{}", change.tag(), change);
                    }
                }
            }
        } else {
            history.push(request.clone());
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

fn from_str<T: for<'de> Deserialize<'de>>(s: &str) -> Option<T> {
    serde_yaml::from_str(&s).ok()
}

fn parse_set_cmd(cmd: &str, options: &mut RuleOptions) -> Option<()> {
    let cmd = cmd.trim();
    match cmd {
        "default" => {
            *options = RuleOptions::NADRIS_PROPOSAL;
            return Some(());
        }
        "permissive" => {
            *options = RuleOptions::PERMISSIVE;
            return Some(());
        }
        "stateless" => {
            *options = RuleOptions::STATELESS;
            return Some(());
        }
        "stable_rust" => {
            *options = RuleOptions::STABLE_RUST;
            return Some(());
        }
        _ => {}
    }
    let cmd = cmd.split(" ").collect_vec();
    let ([opt, val] | [opt, "=", val]) = cmd.as_slice() else {
        return None;
    };
    match *opt {
        "ref_on_ref" => options.ref_on_ref = from_str(val)?,
        "mut_on_ref" => options.mut_on_ref = from_str(val)?,
        "allow_ref_pat_on_ref_mut" => options.allow_ref_pat_on_ref_mut = from_str(val)?,
        "simplify_expressions" => options.simplify_expressions = from_str(val)?,
        "eat_two_layers" => options.eat_two_layers = from_str(val)?,
        "eat_inherited_ref_alone" => options.eat_inherited_ref_alone = from_str(val)?,
        _ => return None,
    }
    Some(())
}
