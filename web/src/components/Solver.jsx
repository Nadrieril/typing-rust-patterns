import init, { RuleOptions, trace_solver_str } from "../../typing_rust_patterns/typing_rust_patterns.js";
import { useState, useMemo } from 'react';
import SolverOptions from './SolverOptions.jsx';

await init({});

// TODO: tab the options container to support bm-based Solver
// TODO: add second column for comparison
// TODO: `rules` tab, including for bm-based
// TODO: encode current view in URL for sharing
export default function Solver() {
    const [options, setOptions] = useState(new RuleOptions());
    const [inputPattern, setInputPattern] = useState("[&x]: &mut [&T]");

    const runSolver = (input) => {
        const result = trace_solver_str(input, options);
        const __html = result.replaceAll("\n", "<br/>");
        return {__html}
    };

    return (
        <>
        <SolverOptions options={options} setOptions={setOptions}/>
        <div style={{display: "flex"}}>
            <label htmlFor="input">Query:</label>
            <input
                type="text"
                id="input"
                spellCheck="false"
                style={{flexGrow: 1}}
                value={inputPattern}
                onChange={(e) => setInputPattern(e.target.value)}
            />
        </div>
        <div id="terminal" dangerouslySetInnerHTML={runSolver(inputPattern)}/>
        </>
    );
}
