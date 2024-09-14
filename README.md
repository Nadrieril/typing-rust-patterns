# Type-based typing of rust patterns

This repo showcases a formalization of the type inference of patterns in rust. It is formalized as
type inference rules, this repo implements a little solver that applies these typing rules to
a given pattern. This can be used to compare alternative rules for typing patterns.

This was built as part of [the initiative][ergo2024] to improve the rules of match ergonomics. The
default settings of the tool is a proposal I (@Nadrieril) [am putting forward][overhaul_rfc] (though
note that this RFC draft is outdated compared to the tool).

## Usage

Run `cargo run` to run the tool. It provides an interactive CLI interface with instructions. Type
`set` to see the various options.

The ruleset in my [RFC draft][overhaul_rfc] is called `stateless` in the tool. This is capable of
emulating stable rust behavior, RFC3627 behavior, as well as a number of alternative proposals.

## Typing rules

The tool answers the basic question: can pattern `<pattern>` work on type `<type>`, and if so what
types are assigned to the bindings inside `<pattern>`? Type `<pattern>: <type>` inside the tool to
get the answer, with reasoning steps.

Available patterns are:
- bindings `x`, `ref y`, `mut z`, etc
- references `&p`, `&mut p`
- tuples, written `[p]`, `[p, q]`

Available types are:
- variables `T`, `U`, etc
- references `&T`, `&mut T`
- tuples, written `[T]`, `[T, U]`

A query to the tool looks like `<pattern>: <type>`, which the tool will then attempt to typecheck.
The internal steps however are presented in terms of a more complex predicate, that looks like `r,
m ⊢ p: T`, where:
- `r` is `inh` or `real` and indicates whether the outermost reference type (if any) is inherited or not;
- `m` is `rw` or `ro` and indicates whether we have mutable or read-only access to the original scrutinee;
- `p` is a pattern;
- `T` is a type.

The initial predicate will always be `real, rw ⊢ <pattern>: <type>`. From there, the solver will
apply typing rules.

Type `rules` to see the current set of typing rules. A rule consists of a list of predicates at the
top called "preconditions", a divider line with the name of the rule, and a list of predicates at
the bottom called "postconditions". To apply a rule, find which postconditions apply to your case
and replace them with the preconditions. Repeat until either no rule applied (this means the input
was a type error), or everything has been resolved into let-bindings.

At the end, you get either a type error or an assignment for all the bindings. The bindings are
assigned a type as well as an expression such as `&(*s).0`, where `s` represents the value that was
matched on.

[overhaul_rfc]: https://hackmd.io/eJdp4f0iQASg5BEPVkCD8g
[typing_rules]: https://hackmd.io/aL5FRz-QTc6K0qtUzPoU9A?view=#Typing-rules
[ergo2024]: https://github.com/rust-lang/rfcs/pull/3627
