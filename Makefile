OUTPUT_DIR=web/typing_rust_patterns

.PHONY: wasm
wasm:
	rm -rf $(OUTPUT_DIR)
	wasm-pack build --target web --release -d $(OUTPUT_DIR)
	# mv $(OUTPUT_DIR)/typing_rust_patterns_bg.wasm $(OUTPUT_DIR)/typing_rust_patterns_bg.big.wasm
	# wasm-opt -Oz $(OUTPUT_DIR)/typing_rust_patterns_bg.big.wasm -o $(OUTPUT_DIR)/typing_rust_patterns_bg.wasm

.PHONY: web-dev
web-dev: wasm
	cd web && pnpm dev

.PHONY: web-lint
web-lint:
	cd web && pnpm check

.PHONY: web-build-dist
web-build-dist: wasm
	cd web && pnpm build

.PHONY: web-build-dist
web-serve-dist: web-build-dist
	cd web/dist && ln -sf . typing-rust-patterns && python -m http.server
