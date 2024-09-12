import init, { RuleOptions, trace_solver_str } from "../../typing_rust_patterns/typing_rust_patterns.js";
import SolverOptions from './SolverOptions.jsx';

import { useState, useMemo } from 'react';
import Tabs from 'react-bootstrap/Tabs';
import Tab from 'react-bootstrap/Tab';

await init({});

// TODO: tab the options container to support bm-based Solver
// TODO: add second column for comparison
// TODO: `rules` tab, including for bm-based
// TODO: encode current view in URL for sharing
export default function Solver() {
    const [options, setOptions] = useState(new RuleOptions());
    const [inputPattern, setInputPattern] = useState("[&x]: &mut [&T]");
    const [mode, setMode] = useState('typechecker');

    const solverSteps = useMemo(() => {
        const result = trace_solver_str(inputPattern, options);
        const __html = result.replaceAll("\n", "<br/>");
        return {__html}
    }, [inputPattern, options]);

    const rulesDisplay = useMemo(() => {
        const result = options.display_rules_js();
        const __html = result.replaceAll("\n", "<br/>");
        return {__html}
    }, [options]);

    return (
        <>
        <SolverOptions options={options} setOptions={setOptions}/>
        <Tabs
            activeKey={mode}
            onSelect={(k) => setMode(k)}
            transition={false}
            className="mb-3"
            fill
        >
            <Tab eventKey="typechecker" title="Typechecker">
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
                <div dangerouslySetInnerHTML={solverSteps}/>
            </Tab>
            <Tab eventKey="rules" title="Rules">
                <div dangerouslySetInnerHTML={rulesDisplay}/>
            </Tab>
        </Tabs>
        </>
    );
}
