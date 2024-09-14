use std::fmt::Display;
use std::fmt::Write;
use std::ops::ControlFlow;

use anyhow::bail;
use colored::Color;
use indoc::indoc;
use itertools::Itertools;

use crate::*;

pub struct CliState {
    pub history: Vec<String>,
    pub options: RuleOptions,
    pub predicate_style: PredicateStyle,
    pub saved: Option<RuleOptions>,
}

impl CliState {
    pub const COMMANDS: &[(&str, &str)] = &[
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

    pub const CLI_OPTIONS: &[OptionsDoc] = &[OptionsDoc {
        name: "predicate_style",
        values: &[
            "Expression",
            "Sequent",
            "BindingMode",
            "SequentBindingMode",
            "Stateless",
        ],
        doc: "the style of the typing predicate; not all rulesets can be expressed in all styles, \
        only `Expression` is compatible with all rulesets",
    }];

    pub fn new() -> Self {
        Self {
            history: Vec::new(),
            options: RuleOptions::NADRI,
            predicate_style: PredicateStyle::Sequent,
            saved: None,
        }
    }

    pub fn settings() -> impl Iterator<Item = OptionsDoc> {
        RuleOptions::OPTIONS_DOC
            .into_iter()
            .chain(Self::CLI_OPTIONS)
            .copied()
    }

    pub fn set_key(&mut self, key: &str, val: &str) -> anyhow::Result<()> {
        if let Some(opt) = Self::settings().find(|opt| opt.name == key) {
            if opt.values.contains(&val) {
                if RuleOptions::OPTIONS_DOC
                    .iter()
                    .find(|opt| opt.name == key)
                    .is_some()
                {
                    self.options.set_key(key, val);
                } else if key == "predicate_style" {
                    self.predicate_style = serde_yaml::from_str(val)?;
                } else {
                    bail!("oops, forgot to implement `set {key}`; please open an issue")
                }
                Ok(())
            } else {
                bail!(
                    "unknown value `{val}` for option `{key}`; options are: {}",
                    opt.values.iter().format(", ")
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
        let style = self.predicate_style;
        let arenas = &Arenas::default();
        let joint_rules = compute_joint_rules(arenas, left, right);

        let mut out = String::new();
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
            out += &DiffState::side_by_side(&left, &right);
            let _ = writeln!(&mut out);
        }
        Ok(out)
    }

    pub fn step(&mut self, request: &str) -> anyhow::Result<ControlFlow<()>> {
        let request = request.trim();
        if request == "?" || request == "help" {
            println!(
                indoc!(
                    "
                Commands: {}

                {}
                "
                ),
                CliState::COMMANDS.iter().map(|(name, _)| name).format(", "),
                CliState::COMMANDS
                    .iter()
                    .map(|(name, doc)| format!("- {name}: {doc}"))
                    .format("\n"),
            )
        } else if request == "q" || request == "quit" {
            return Ok(ControlFlow::Break(()));
        } else if request == "options" {
            if let Some(saved) = self.saved {
                println!("Comparing against the saved ruleset. Use `unsave` to forget the saved ruleset.");
                println!("The current ruleset is on the left, and the saved one on the right.");
                display_options_diff(self.options, saved);
            } else {
                let style = serde_yaml::to_string(&self.predicate_style)?;
                print!("predicate_style: {}", style);
                display_options_diff(self.options, self.options);
            }
        } else if request == "rules" {
            if let Some(saved) = self.saved {
                if saved == self.options {
                    println!(indoc!(
                        "
                        This ruleset is the same as the one that was previously saved. Change some
                        settings and run `rules` again.
                        "
                    ))
                } else {
                    println!("Comparing against the saved ruleset. Use `unsave` to forget the saved ruleset.");
                    println!("The two rulesets are described by the following sets of rules, with differences highlighted.");
                    println!("The current ruleset is on the left, and the saved one on the right.");
                    println!();
                    let s = self.display_joint_rules(self.options, saved).unwrap();
                    print!("{s}");
                }
            } else {
                println!(
                    "The current options can be fully described as the following set of rules."
                );
                println!("{}", explain_predicate(self.predicate_style));
                print!(
                    "{}",
                    display_rules(self.predicate_style, self.options).unwrap()
                );
            }
        } else if request == "save" {
            println!("Current ruleset was saved");
            self.saved = Some(self.options);
        } else if request == "unsave" {
            println!("Saved ruleset was forgotten");
            self.saved = None;
        } else if request == "swap" {
            if let Some(saved) = &mut self.saved {
                println!("Current and saved rulesets were swapped");
                std::mem::swap(saved, &mut self.options);
                let saved = *saved;
                println!("The two rulesets are described by the following sets of rules, with differences highlighted.");
                println!(
                    "The old current ruleset is on the left, and the new current one on the right."
                );
                println!();
                let s = self.display_joint_rules(saved, self.options).unwrap();
                println!("{s}");
                display_options_diff(saved, self.options);
            } else {
                println!("Can't swap saved and current ruleset because there is no saved ruleset. Use `save` to save one.");
            }
        } else if request == "compare" {
            if let Some(saved) = self.saved {
                if saved == self.options {
                    println!(indoc!(
                        "
                        This ruleset is the same as the one that was previously saved. Change some
                        settings and run `compare` again.
                        "
                    ))
                } else {
                    // TODO: show sets of options at the top
                    let a = &Arenas::default();
                    let differences = compare_rulesets(
                        a,
                        3,
                        4,
                        RuleSet::TypeBased(saved),
                        std::cmp::Ordering::Equal,
                        RuleSet::TypeBased(self.options),
                    );

                    if differences.is_empty() {
                        println!(
                            "The rulesets are identical on all patterns \
                            of depth <= 3 and types of depth <= 4"
                        );
                    } else {
                        for (test_case, left_res, right_res) in differences {
                            let test_case_str = test_case.to_string();
                            println!("Difference on `{test_case_str}`:");
                            println!("    saved returned: {left_res}");
                            println!("  current returned: {right_res}");
                        }
                    }
                }
            } else {
                println!("Can't compare rulesets because there is no saved ruleset. Use `save` to save one.");
            }
        } else if let Some(cmd) = request.strip_prefix("set") {
            self.history.push(request.to_string());
            let old_style = self.predicate_style;
            let old_options = self.options;
            match parse_set_cmd(cmd, self) {
                // Display what changed.
                Ok(_) => {
                    if matches!(self.predicate_style, PredicateStyle::SequentBindingMode) {
                        // The `SequentBindingMode` style cannot work without `always_inspect_bm`.
                        if !matches!(old_style, PredicateStyle::SequentBindingMode) {
                            self.options.always_inspect_bm = true;
                        } else if old_options.always_inspect_bm && !self.options.always_inspect_bm {
                            self.predicate_style = PredicateStyle::BindingMode;
                        }
                    }
                    if old_options != self.options {
                        println!("The two rulesets are described by the following sets of rules, with differences highlighted.");
                        println!("The old ruleset is on the left, and the new one on the right.");
                        println!();
                        match self.display_joint_rules(old_options, self.options) {
                            Ok(s) => {
                                println!("{s}");
                                display_options_diff(old_options, self.options);
                            }
                            Err(IncompatibleStyle) => {
                                self.options = old_options;
                                print!(
                                    "Error: the new ruleset cannot be displayed with style {}. ",
                                    self.predicate_style
                                );
                                println!("Change the style and try again.");
                            }
                        }
                    } else {
                        if display_rules(self.predicate_style, self.options).is_err() {
                            println!(
                                "Error: the current ruleset cannot be displayed with style {}.",
                                self.predicate_style
                            );
                            self.predicate_style = old_style;
                        } else if self.saved.is_some_and(|saved| {
                            display_rules(self.predicate_style, saved).is_err()
                        }) {
                            println!(
                                "Error: the saved ruleset cannot be displayed with style {}.",
                                self.predicate_style
                            );
                            self.predicate_style = old_style;
                        }
                    }
                }
                Err(err) => {
                    let options = CliState::settings()
                        .map(|OptionsDoc { name, values, doc }| {
                            format!("- {name}: {}\n    {doc}\n", values.iter().format(" | "))
                        })
                        .format("");
                    let bundles = RuleOptions::KNOWN_OPTION_BUNDLES
                        .iter()
                        .map(|b| format!("- {}: {}", b.name, b.doc))
                        .format("\n");
                    println!(
                        "Error: {err}\n\n\
                        Options are:\n\
                        {options}\n\
                        There also exist some predefined option-bundles. Activate one with `set <bundle>`\n\
                        {bundles}",
                    )
                }
            }
        } else {
            self.history.push(request.to_string());
            let a = &Arenas::default();
            let request = match TypingRequest::parse(a, request) {
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
                        - references `&T`, `&mut T`\n\
                        - tuples `[T]`, `[T, U]`"
                    );
                    return Ok(ControlFlow::Continue(()));
                }
                Ok(request) => request,
            };

            if let Some(saved) = self.saved {
                println!("Comparing against the saved ruleset. Use `unsave` to forget the saved ruleset.");
                println!("The current ruleset is on the left, and the saved one on the right.");
                let left = trace_solver(request, self.options, self.predicate_style);
                let right = trace_solver(request, saved, self.predicate_style);
                let traces = DiffState::side_by_side(&left, &right);
                println!("{traces}")
            } else {
                let trace = trace_solver(request, self.options, self.predicate_style);
                println!("{trace}")
            }
        }
        Ok(ControlFlow::Continue(()))
    }
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
    use colored::Colorize;
    let old_options = old_options.to_map();
    let new_options = new_options.to_map();
    for (k, v) in &old_options {
        let new_v = &new_options[k];
        if v == new_v {
            println!("{k}: {v}");
        } else {
            println!(
                "{k}: {} -> {}",
                v.to_string().color(Color::Red),
                new_v.to_string().color(Color::Green)
            );
        }
    }
}

pub fn explain_predicate(style: PredicateStyle) -> String {
    let mut out = String::new();
    let _ = writeln!(
        &mut out,
        "The typing predicate looks like `{}`, where",
        TypingPredicate::ABSTRACT.display(style),
    );
    match style {
        PredicateStyle::Expression | PredicateStyle::BindingMode => {
            let _ = writeln!(&mut out, "- `e` is an expression",);
        }
        PredicateStyle::Sequent => {
            let _ = writeln!(
                &mut out,
                "- `r` is `inh` or `real` and indicates whether the outermost reference type (if any) is inherited or not;",
            );
            let _ = writeln!(&mut out, "- `m` is `rw` or `ro` and indicates whether we have mutable or read-only access to the original scrutinee;",);
        }
        PredicateStyle::SequentBindingMode => {
            let _ = writeln!(
                &mut out,
                "- `bm` is `move`, `ref` or `ref mut` and indicates the binding mode;",
            );
            let _ = writeln!(&mut out, "- `m` is `rw` or `ro` and indicates whether we have mutable or read-only access to the original scrutinee;",);
        }
        PredicateStyle::Stateless => {}
    }
    let _ = writeln!(&mut out, "- `p` is a pattern;");
    let _ = writeln!(&mut out, "- `T` is a type.");
    out
}

pub fn display_rules(
    style: PredicateStyle,
    options: RuleOptions,
) -> Result<String, IncompatibleStyle> {
    let arenas = &Arenas::default();
    let ctx = TypingCtx { arenas, options };
    let typing_rules = compute_rules(ctx);
    let mut out = String::new();
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

    fn side_by_side(left: &str, right: &str) -> String {
        use DiffState::*;
        let mut out = String::new();
        for x in left.lines().zip_longest(right.lines()) {
            let (l, r) = x.or(" ", " ");
            let same = l == r;
            let l_state = if same { Both } else { Old };
            let r_state = if same { Both } else { New };
            let l = l_state.color_line(l);
            let r = r_state.color_line(r);
            let _ = writeln!(&mut out, " {l:80} | {r}");
        }
        out
    }
}
