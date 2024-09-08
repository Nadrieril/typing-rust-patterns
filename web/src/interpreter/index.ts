/**
 * Wrapper of the Miri interpreter.
 * 
 * Runs the code in a Web Worker to minimize slowdowns.
 * 
 * To interact with the Interpreter use the `run` and `onResult` methods.
 */
export class Interpreter {
  private readonly worker: Worker
  private onrun: (() => void)[];
  private onresult: ((result: string) => void)[]
  private onloaded: (() => void)[];
  
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
    this.onrun = [];
    this.onresult = [];
    this.onloaded = [];

    this.worker.onmessage = ({ data }: { data: { loaded?: boolean, result?: string } } ) => {
      if (data.result !== undefined) {
        for (const ev of this.onresult) {
          ev(data.result)
        }
      } else if (data.loaded !== undefined) {
        for (const ev of this.onloaded) {
          ev()
        }
      } else {
        console.log(`Received message`, data)
      }
    };
  }
  
  /**
   * Execute a snippet of code with Miri.
   * 
   * The code is automatically wrapped with a `main` function.
   * 
   * @param code Code that will be interpreted.
   */
  public run(code: string, printLast: boolean = false, inherited_ref_on_ref: string) {
    for (const ev of this.onrun) {
      ev()
    }
    this.worker.postMessage({code, printLast, inherited_ref_on_ref});
  }
  
  /**
   * @param onrun Called when the Interpreter executes some code.
   */
  public onRun(onrun: () => void) {
    this.onrun.push(onrun);
  }
  
  /**
   * @param onresult Called when the Interpreter has finished executing code and has a result.
   */
  public onResult(onresult: (result: string) => void) {
    this.onresult.push(onresult);
  }
  
  /**
   * @param onloaded Called when the Interpreter has finished loading.
   */
  public onLoaded(onloaded: () => void) {
    this.onloaded.push(onloaded);
  }
}
