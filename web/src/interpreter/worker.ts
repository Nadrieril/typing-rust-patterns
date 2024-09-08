import init, { RuleOptions, trace_solver_str } from "../../typing_rust_patterns/typing_rust_patterns.js";

export async function initInterpreter(): Promise<Interpreter> {
  console.time("init");
  await init({});
  console.timeEnd("init");
  return new Interpreter();
}

class Interpreter {
  constructor() {}

  async run(code: string, printLast: boolean = false, inherited_ref_on_ref: string): Promise<string> {
    console.time("solver execution");
    try {
      let options = new RuleOptions();
      options.set_key("inherited_ref_on_ref", inherited_ref_on_ref);
      const out = trace_solver_str(code, options);
      return out

    } catch (e: any) {
      return e.message;

    } finally {
      console.timeEnd("solver execution");
    };
  }
}

(async () => {
  // Build main Interpreter
  const interpreter = await initInterpreter();
  
  // When code is received run it
  addEventListener("message", async (event) => {
    const {
      code, 
      printLast,
      inherited_ref_on_ref,
    } = event.data;
    const result = await interpreter.run(code, printLast, inherited_ref_on_ref);
    postMessage({ result });
  });
  
  // Send a message when finished loading
  postMessage({ loaded: true })
})()
