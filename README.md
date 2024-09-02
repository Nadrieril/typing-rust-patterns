# Type-based typing of rust patterns

Match ergonomics is a feature of rust that allows patterns to seamlessly work through references. It
is extremely useful but not simple in its workings. There is at the moment [an initiative][ergo2024]
to improve these rules. While it does improve ergonomics, it has revealed how complex the currently
accepted mental model ("default binding modes") is to handle correctly.

Put simply, I think this mental model is the wrong one. It does not align with how users naturally
think about patterns, it is not easy to hold in your head, and it falls short of the sharpness of
design I have come to expect from rust.

I propose instead a mental model based on types: when e.g. `Some(x)` is matched against
`&Option<T>`, the reference "slides into" the pattern, as if we were matching on `Option<&T>`. This
can be made formal by expressing it as typing rules, which I present in [this RFC draft][overhaul_rfc].

This repo implements a little solver that applies these typing rules to a given pattern. It is
fairly easy to use: type `cargo run` and follow the instructions. The default setting is one
potential proposal I am putting forward; type `set` to see the various options. The ruleset in the
[RFC][overhaul_rfc] is called `stateless` in the tool. This is capable of emulating stable rust
behavior, RFC3627 behavior, as well as a number of alternative proposals.

## Typing rules

The basic premise of the tool is to formalize type checking of patterns as typing rules. The
currently enabled rules can be visualized by typing `rules` in the prompt. A rule consists of a list
of predicates at the top called "preconditions", a divider line with the name of the rule, and
a list of predicates at the bottom called "postconditions". To apply a rule, find which
postconditions apply to your case and replace them with the preconditions. Repeat until either no
rule applied (this means the input was a type error), or everything has been resolved into `let
<binding>: <type> = <expression>;` bindings.

A query to the tool looks like `<pattern>: <type>`, which the tool will then attempt to typecheck.
The rules however are presented in terms of a more complex predicate, that looks like `<pattern>
@ <expression> : <type>`. The expression is a clever way to track what the current subpattern is
accessing. The initial expression (written `s`) is called "the scrutinee". As we apply rules, the
expression makes it possible to know the current binding mode, to speak about whether the scrutinee
is accessible mutably or not, and to borrow-check the final result.

All this makes it possible to implement RFCs 2005 and 3627 as well as their variants. Note that the
[proposal][overhaul_rfc] that comes with this tool does not need this expression; the only state it
tracks is the type. This can be checked by typing `set stateless` (`stateless` is the name of the
ruleset put forward in that RFC), then `rules` to see that none of the rules depend on the
expression.

[overhaul_rfc]: https://hackmd.io/eJdp4f0iQASg5BEPVkCD8g
[typing_rules]: https://hackmd.io/aL5FRz-QTc6K0qtUzPoU9A?view=#Typing-rules
[ergo2024]: https://github.com/rust-lang/rfcs/pull/3627
