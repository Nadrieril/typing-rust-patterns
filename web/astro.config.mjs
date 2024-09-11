import { defineConfig } from 'astro/config';
import compress from "astro-compress";
import compressor from "astro-compressor";
import react from '@astrojs/react';

// https://astro.build/config
export default defineConfig({
  site: "https://nadrieril.github.io",
  base: "/typing-rust-patterns",
  integrations: [react(), compress(), compressor({
    brotli: false,
    fileExtensions: [".css", ".js", ".html", ".xml", ".cjs", ".mjs", ".svg", ".txt", ".wasm"]
  })]
});
