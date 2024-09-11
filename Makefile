OUTPUT_DIR=web/typing_rust_patterns

.PHONY: wasm
wasm:
	rm -rf $(OUTPUT_DIR)
	wasm-pack build --target web --release -d $(OUTPUT_DIR)
	# mv $(OUTPUT_DIR)/typing_rust_patterns_bg.wasm $(OUTPUT_DIR)/typing_rust_patterns_bg.big.wasm
	# wasm-opt -Oz $(OUTPUT_DIR)/typing_rust_patterns_bg.big.wasm -o $(OUTPUT_DIR)/typing_rust_patterns_bg.wasm
