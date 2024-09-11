import init, { RuleOptions, trace_solver_str } from "../../typing_rust_patterns/typing_rust_patterns.js";
import { useState, useMemo, Fragment } from 'react';
import Button from 'react-bootstrap/Button';
import ButtonGroup from 'react-bootstrap/ButtonGroup';

await init({});

const optionsDoc = RuleOptions.options_doc();

function InhRef() {
    return <span
        style={{color: "gray", textDecoration: "underline"}}
        title="inherited reference"
    >
        &
    </span>
}
const prettyOptions = {
    match_constructor_through_ref: {
        question: <>[p]: &[T]</>,
        true: <>p: <InhRef/>T</>,
        false: "❌",
    },
    eat_inherited_ref_alone: {
        question: <>&p: <InhRef/>[T]</>,
        true: <>p: [T]</>,
        false: "❌",
    },
    inherited_ref_on_ref: {
        question: <>&p: <InhRef/>&T</>,
        EatOuter: <><span style={{color: "red"}} title="reference to consider">&</span>&T</>,
        EatInner: <>&<span style={{color: "red"}} title="reference to consider">&</span>T</>,
        EatBoth: <><span style={{color: "red"}} title="reference to consider">&&</span>T</>,
    },
    fallback_to_outer: {
        question: <>&p: <InhRef/>&T is ❌</>,
        true: <><span style={{color: "red"}} title="reference to consider">&</span>&T</>,
        false: "❌",
    },
    dont_eat_mut_inside_shared: {
        question: <>&mut p: <InhRef/><span style={{color: "red"}} title="reference to consider">&mut </span>T</>,
        false: "✅",
        true: "❌",
    },
    downgrade_mut_inside_shared: {
        question: <>&...&mut T</>,
        true: <>&...&T</>,
        false: <>&...&mut T</>,
    },
    allow_ref_pat_on_ref_mut: {
        question: <>&p: &mut T</>,
        true: <>&p: &T</>,
        false: "❌",
    },
    ref_binding_on_inherited: {
        question: <>ref x: <InhRef/>T</>,
        ResetBindingMode: <>x: &T</>,
        AllocTemporary: <>x: &&T</>,
        Error: "❌",
    },
    mut_binding_on_inherited: {
        question: <>mut x: <InhRef/>T</>,
        ResetBindingMode: <>mut x: T</>,
        Keep: <>mut x: &T</>,
        Error: "❌",
    },
    // TODO: hide that one? pass it as separate option to solver?
    always_inspect_bm: {
        question: "always inspect bm",
        true: "✅",
        false: "❌",
    },
    // TODO: hide that one?
    simplify_deref_mut: {
        question: "simplify_deref_mut",
        true: "✅",
        false: "❌",
    },
}

// TODO: abstract over options container
// TODO: tab the options container to support bm-based Solver
// TODO: add presets somehow
// TODO: add second column for comparison
// TODO: `rules` tab, including for bm-based
// TODO: encode current view in URL for sharing
export default function Solver() {
    const [options, setOptions] = useState(new RuleOptions());
    const [inputPattern, setInputPattern] = useState("[&x]: &mut [&T]");

    const setKey = (k, v) => setOptions(options.with_key(k, v));

    const runSolver = (input) => {
        const result = trace_solver_str(input, options);
        const __html = result.replaceAll("\n", "<br/>");
        return {__html}
    };

    const option_elems = optionsDoc.map(option => {
        const current_val = options.get_key(option.name);
        const prettyOption = prettyOptions[option.name] || null;
        const question = prettyOption ? prettyOption.question : option.name;
        const label = <label key={option.name} htmlFor={option.name} title={option.name}>{question}</label>;
        const buttons = option.values.map((v) => {
            const name = prettyOption ? prettyOption[v] : v;
            return <Button variant="light"
                key={v}
                active={current_val == v}
                onClick={() => setKey(option.name, v)}
            >
                {name}
            </Button>
        });
        return <tr key={option.name}>
            <td style={{width: "fit-content"}}>{label}</td>
            <td style={{textAlign: "center"}}><ButtonGroup>{buttons}</ButtonGroup></td>
        </tr>
    });

    return (
        <>
        <table id="options"><tbody>{option_elems}</tbody></table>
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
