.PHONY: wasm
wasm:
	rm -r web/typing_rust_patterns
	wasm-pack build --target web --release -d web/typing_rust_patterns
	# mv pkg/typing_rust_patterns_bg.wasm pkg/typing_rust_patterns_bg.big.wasm
	# wasm-opt -Oz pkg/typing_rust_patterns_bg.big.wasm -o pkg/typing_rust_patterns_bg.wasm
