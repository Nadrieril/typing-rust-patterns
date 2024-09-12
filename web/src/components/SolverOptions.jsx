import init, { RuleOptions } from "../../typing_rust_patterns/typing_rust_patterns.js";
import Button from 'react-bootstrap/Button';
import ButtonGroup from 'react-bootstrap/ButtonGroup';
import Dropdown from 'react-bootstrap/Dropdown';
import Table from 'react-bootstrap/Table';

await init({});

const optionsDoc =
    RuleOptions
        .options_doc()
        .filter((opt) => opt.name != "simplify_deref_mut" && opt.name != "always_inspect_bm");

const bundlesDoc = RuleOptions.bundles_doc();

function InhRef() {
    return <span className="inherited-ref" title="inherited reference">&</span>
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
        question: <>p: &...&mut T</>,
        true: <>p: &...&T</>,
        false: <>p: &...&mut T</>,
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
}

export default function SolverOptions({ options, setOptions }) {
    const setKey = (k, v) => setOptions(options.with_key(k, v));

    const option_elems = optionsDoc.map(option => {
        const current_val = options.get_key(option.name);
        const prettyOption = prettyOptions[option.name] || null;
        const question = prettyOption ? prettyOption.question : option.name;
        const label = <label key={option.name} htmlFor={option.name} title={option.doc}>{question}</label>;
        const buttons = option.values.map((v) => {
            const name = prettyOption ? prettyOption[v] : v;
            return <Button variant="light"
                key={v}
                title={v}
                active={current_val == v}
                onClick={() => setKey(option.name, v)}
            >
                {name}
            </Button>
        });
        return <tr key={option.name}>
            <td style={{width: "fit-content"}}>{label}</td>
            <td style={{textAlign: "left"}}><ButtonGroup>{buttons}</ButtonGroup></td>
        </tr>
    });

    const active_bundle = options.get_bundle_name_js();
    const bundles = bundlesDoc.map(bundle => {
        return <Dropdown.Item
            key={bundle.name}
            title={bundle.name}
            active={active_bundle == bundle.name}
            onClick={() => setOptions(bundle.options)}
        >
            {bundle.name}
        </Dropdown.Item>
    });

    return (
        <div>
        <Dropdown>
            <label htmlFor="presets">Preset: </label>
            <Dropdown.Toggle variant="outline-dark" id="presets">
                {active_bundle || "---"}
            </Dropdown.Toggle>

            <Dropdown.Menu>{bundles}</Dropdown.Menu>
        </Dropdown>
        <Table bordered hover><tbody>{option_elems}</tbody></Table>
        </div>
    );
}
