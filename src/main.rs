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

    let mut options = RuleOptions {
        ref_on_ref: RefOnRefBehavior::Skip,
        ..RuleOptions::PERMISSIVE
    };

    let mut history = Vec::new();
    let prompt = |history: &[_]| {
        Text::new("")
            .with_history(SimpleHistory::new(history.iter().rev().cloned().collect()))
            .prompt_skippable()
    };
    while let Some(request) = prompt(&history)? {
        if request == "?" || request == "help" {
            println!("Commands: options, set");
        } else if request == "options" || request == "option" {
            let options = serde_yaml::to_string(&options)?;
            print!("{options}");
        } else if let Some(cmd) = request.strip_prefix("set") {
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
                    - eat_inherited_ref: bool\n    \
                        whether `&p: &T` is allowed if the reference is inherited and `T` isn't some `&U`"
                )
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

/// Horrible hack to get the enum variant from its variant name x)
fn from_str<T: for<'de> Deserialize<'de>>(s: &str) -> Option<T> {
    #[derive(Deserialize)]
    struct Helper<T> {
        field: T,
    }
    let s = format!("field: {s}");
    serde_yaml::from_str(&s).ok().map(|h: Helper<T>| h.field)
}

fn parse_set_cmd(cmd: &str, options: &mut RuleOptions) -> Option<()> {
    let cmd = cmd.trim();
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
        "eat_inherited_ref" => options.eat_inherited_ref = from_str(val)?,
        _ => return None,
    }
    Some(())
}
