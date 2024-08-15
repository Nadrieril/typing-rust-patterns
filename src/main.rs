use inquire::history::SimpleHistory;
use inquire::Text;

use typing_rust_patterns::*;

fn main() -> anyhow::Result<()> {
    println!("Welcome to the interactive pattern typer!");
    println!("Write `pattern: type` on the prompt line and I will attempt to type it.");
    println!("Example: `&[ref x]: &[T]`");
    println!("");

    let options = RuleOptions {
        ref_on_ref: RefOnRefBehavior::Skip,
        mut_on_ref: MutOnRefBehavior::Keep,
        allow_ref_pat_on_ref_mut: true,
        simplify_expressions: true,
    };

    let mut history = Vec::new();
    loop {
        let prompt =
            Text::new("").with_history(SimpleHistory::new(history.iter().rev().cloned().collect()));
        match prompt.prompt_skippable()? {
            Some(request) => {
                history.push(request.clone());
                match trace_solver(&request, options) {
                    Ok(trace) => println!("{trace}"),
                    Err(err) => println!("Parse error: `{err}`"),
                }
            }
            None => break,
        }
    }
    Ok(())
}
