import init, { RuleOptions, trace_solver_str } from "../../typing_rust_patterns/typing_rust_patterns.js";

(async () => {
    console.time("init");
    await init({});
    console.timeEnd("init");

    let options = new RuleOptions();

    // When code is received run it
    addEventListener("message", async (event) => {
        const data = event.data;
        switch (data.call) {
            case "set_key":
                console.log("Setting key", data.key, ":", data.value);
                options.set_key(data.key, data.value);
                break;

            case "run_solver":
                console.time("run_solver");
                let result = "unknown error";
                try {
                    result = trace_solver_str(data.code, options);
                } catch (e: any) {
                    result = e.message;
                } finally {
                    console.timeEnd("run_solver");
                };
                postMessage({ event: "result", result });
                break;
        }
    });

    // Send a message when finished loading
    postMessage({ event: "loaded" })
})()
