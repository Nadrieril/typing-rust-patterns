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
can be made formal by expressing it as typing rules, which I have done [here][typing_rules].

This repo implements a little solver that applies these typing rules to a given pattern. It is
fairly easy to use: type `cargo run` and follow the instructions. The default setting is one
potential proposal I am putting forward; type `options` to see various options. This is capable of
emulating stable rust behavior, and I'm working on handling the various options of the 2024
initiative.

[typing_rules]: https://hackmd.io/aL5FRz-QTc6K0qtUzPoU9A?view=#Typing-rules
[ergo2024]: https://github.com/rust-lang/rfcs/pull/3627
