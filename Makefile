.PHONY: wasm
wasm:
	wasm-pack build --target web --release
	# mv pkg/typing_rust_patterns_bg.wasm pkg/typing_rust_patterns_bg.big.wasm
	# wasm-opt -Oz pkg/typing_rust_patterns_bg.big.wasm -o pkg/typing_rust_patterns_bg.wasm
