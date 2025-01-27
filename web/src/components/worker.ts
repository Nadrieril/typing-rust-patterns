import init, { RuleSetJs, compare_rulesets_js } from "../../typing_rust_patterns/typing_rust_patterns.js";

(async () => {
    await init({});

    addEventListener("message", async (event) => {
        const data = event.data;
        switch (data.type) {
            case "compare":
                const output = compare_rulesets_js(
                    RuleSetJs.decode(data.optionsLeft)!,
                    RuleSetJs.decode(data.optionsRight)!,
                    data.patDepth,
                    data.tyDepth,
                    data.compareDirection
                );
                postMessage({ type: "compare", output });
                break;
        }
    });

    postMessage({ type: "loaded" });
})()
