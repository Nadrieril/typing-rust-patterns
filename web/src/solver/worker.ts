import init, { RuleOptions, trace_solver_str } from "../../typing_rust_patterns/typing_rust_patterns.js";

export async function initSolver(): Promise<Solver> {
  console.time("init");
  await init({});
  console.timeEnd("init");
  return new Solver();
}

class Solver {
  options: RuleOptions;

  constructor() {
    this.options = new RuleOptions();
  }

  async run(code: string): Promise<string> {
    console.time("solver execution");
    try {
      return trace_solver_str(code, this.options)

    } catch (e: any) {
      return e.message;

    } finally {
      console.timeEnd("solver execution");
    };
  }

  set_key(key: string, value: string) {
    console.log("Setting key", key, ":", value);
    this.options.set_key(key, value);
  }
}

(async () => {
  // Build main Solver
  const solver = await initSolver();
  
  // When code is received run it
  addEventListener("message", async (event) => {
    const data = event.data;
    if (data.call == "set_key") {
      solver.set_key(data.key, data.value);
    } else if (data.call == "run_solver") {
      const result = await solver.run(data.code);
      postMessage({ result });
    }
  });

  // Send a message when finished loading
  postMessage({ loaded: true })
})()
