use std::io::IsTerminal;

use anyhow::bail;
use inquire::{history::SimpleHistory, Text};

use itertools::Itertools;
use typing_rust_patterns::*;

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

    let mut options = RuleOptions::NADRIS_PROPOSAL;

    let mut history = Vec::new();
    let prompt = |history: &[_]| {
        if is_interactive {
            Text::new("")
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
        if request == "?" || request == "help" {
            println!("Commands: options, set, quit");
        } else if request == "q" || request == "quit" {
            break;
        } else if request == "options" || request == "option" {
            let options = serde_yaml::to_string(&options)?;
            print!("{options}");
        } else if let Some(cmd) = request.strip_prefix("set") {
            let old_options = options;
            if let Err(err) = parse_set_cmd(cmd, &mut options) {
                println!(
                    "Error: {err}\n\n\
                    Options are:\n\
                    {}\n\
                    There also exist some predefined option-bundles. Activate one with `set bundle`\n\
                    {}",
                    RuleOptions::OPTIONS_DOC.iter().map(|(name, ty, descr)| format!("- {name}: {ty}\n    {descr}\n")).format(""),
                    RuleOptions::KNOWN_OPTION_BUNDLES.iter().map(|(name, _, descr)| format!("- {name}: {descr}")).format("\n")
                )
            } else {
                // Display what changed.
                let old_options = old_options.to_map();
                let new_options = options.to_map();
                for (k, v) in &old_options {
                    let new_v = &new_options[k];
                    if v != new_v {
                        println!("{k}: {v} -> {new_v}");
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
