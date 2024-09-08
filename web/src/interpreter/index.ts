/**
 * Wrapper of the interpreter.
 * 
 * Runs the code in a Web Worker to minimize slowdowns.
 * 
 * To interact with the Interpreter use the `run` and `onResult` methods.
 */
export class Interpreter {
  private readonly worker: Worker
  private onresult: ((result: string) => void)[]
  private onloaded: (() => void)[];
  private running: bool;
  
  /**
   * Creates a new Interpreter.
   * 
   * The constructor is asyncronous, check `onLoaded` for status on when it can be used.
   * 
   * @example
   * const interpreter = new Interpreter();
   * interpreter.onLoaded(() => {
   *   interpreter.run('println!("Hello, World!");');
   * })
   */
  constructor() {
    this.worker = new Worker(new URL("./worker.ts", import.meta.url), { type: "module" });
    this.onresult = [];
    this.onloaded = [];
    this.running = false;

    this.worker.onmessage = ({ data }: { data: { loaded?: boolean, result?: string } } ) => {
      if (data.result !== undefined) {
        this.running = false;
        for (const ev of this.onresult) {
          ev(data.result)
        }
      } else if (data.loaded !== undefined) {
        for (const ev of this.onloaded) {
          ev()
        }
      }
    };
  }

  public is_running() {
      return this.running;
  }

  public run_solver(code: string) {
    this.running = true;
    this.worker.postMessage({call: "run_solver", code});
  }

  public set_key(key: string, value: string) {
    this.worker.postMessage({call: "set_key", key, value});
  }

  /**
   * @param onloaded Called when the Interpreter has finished loading.
   */
  public onLoaded(onloaded: () => void) {
    this.onloaded.push(onloaded);
  }

  /**
   * @param onresult Called when the Interpreter has finished executing code and has a result.
   */
  public onResult(onresult: (result: string) => void) {
    this.onresult.push(onresult);
  }
}
