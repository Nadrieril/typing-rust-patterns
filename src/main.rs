use std::io::IsTerminal;
use std::ops::ControlFlow;

use inquire::{history::SimpleHistory, CustomUserError, Text};
use itertools::Itertools;

use typing_rust_patterns::*;

fn main() -> anyhow::Result<()> {
    let mut state = CliState::new();

    let is_interactive = std::io::stdin().is_terminal();
    if is_interactive {
        println!("Welcome to the interactive pattern typer!");
        println!("Write `pattern: type` on the prompt line and I will attempt to type it.");
        println!("Example: `&[ref x]: &[T]`");
        println!("Type `help` for a list of available commands.");
        println!("");
    }

    if is_interactive {
        while let Some(request) = Text::new("")
            .with_placeholder("[&x]: &mut [&T]")
            .with_autocomplete(Autocomplete)
            .with_history(SimpleHistory::new(
                state.history.iter().rev().cloned().collect(),
            ))
            .prompt_skippable()?
        {
            match state.step(&request)? {
                ControlFlow::Break(()) => break,
                ControlFlow::Continue(()) => {}
            }
        }
    } else {
        let mut buffer = String::new();
        while {
            buffer.clear();
            std::io::stdin().read_line(&mut buffer)?;
            !buffer.is_empty()
        } {
            for cmd in buffer.split(";") {
                match state.step(&cmd)? {
                    ControlFlow::Break(()) => break,
                    ControlFlow::Continue(()) => {}
                }
            }
        }
    }
    Ok(())
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

        for &(cmd, _) in CliState::COMMANDS {
            if cmd.starts_with(input) && cmd != input {
                ret.push(format!("{cmd}"))
            }
        }

        if let Some(opt) = input.strip_prefix("set") {
            let opt = opt.trim();
            for OptionsDoc { name, values, .. } in CliState::settings() {
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
            for b in RuleOptions::KNOWN_OPTION_BUNDLES {
                if b.name.starts_with(opt) {
                    ret.push(format!("set {}", b.name))
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
