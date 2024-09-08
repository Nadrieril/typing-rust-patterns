import init, { trace_solver_str } from "../../typing_rust_patterns/typing_rust_patterns.js";

export async function initInterpreter(): Promise<Interpreter> {
  console.time("init");
  await init({});
  console.timeEnd("init");
  return new Interpreter();
}

class Interpreter {
  constructor() {}

  async run(code: string, printLast: boolean = false): Promise<string> {
    try {
      console.time("solver execution");
      const out = trace_solver_str(code);
      console.timeEnd("solver execution");
      return out
    } catch (e: any) {
      return e.message;
    };
  }
}
