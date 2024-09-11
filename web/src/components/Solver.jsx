import init, { RuleOptions, trace_solver_str } from "../../typing_rust_patterns/typing_rust_patterns.js";
import { useState, useMemo, Fragment } from 'react';

await init({});

export default function Solver() {
    const optionsDoc = useMemo(() => RuleOptions.options_doc());
    const [options, setOptions] = useState(new RuleOptions());
    const [inputPattern, setInputPattern] = useState("[&x]: &mut [&T]");

    const setKey = (k, v) => setOptions(options.with_key(k, v));

    const runSolver = (input) => {
        const result = trace_solver_str(input, options);
        const __html = result.replaceAll("\n", "<br/>");
        return {__html}
    };

    const option_elems = optionsDoc.map(option => {
        const label = <label htmlFor={option.name} key={option.name}>{option.name}</label>;
        let select;
        if (option.values.length == 2 && option.values[0] == "true" && option.values[1] == "false") {
            select = <input
                type="checkbox"
                id={option.name}
                checked={options.get_key(option.name) == "true"}
                onChange={(e) => setKey(option.name, e.target.checked ? "true" : "false")}
            />;
        } else {
            const values = option.values.map((v) => <option key={v} value={v}>{v}</option>);
            select = <select
                id={option.name}
                value={options.get_key(option.name)}
                onChange={(e) => setKey(option.name, e.target.value)}
            >
                {values}
            </select>
        };
        return <Fragment key={option.name}>
            {label}
            {select}
            <br/>
        </Fragment>
    });

    return (
        <>
        <div id="options">{option_elems}</div>
        <input
            type="text"
            id="input"
            spellCheck="false"
            value={inputPattern}
            onChange={(e) => setInputPattern(e.target.value)}
        />
        <div id="terminal" dangerouslySetInnerHTML={runSolver(inputPattern)}/>
        </>
    );
}
