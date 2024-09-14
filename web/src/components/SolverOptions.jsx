import init, { RuleOptions } from "../../typing_rust_patterns/typing_rust_patterns.js";
import { useRef, useState, useEffect } from 'react';

import Button from 'react-bootstrap/Button';
import ButtonGroup from 'react-bootstrap/ButtonGroup';
import Col from 'react-bootstrap/Col';
import Container from 'react-bootstrap/Container';
import Dropdown from 'react-bootstrap/Dropdown';
import Form from 'react-bootstrap/Form';
import InputGroup from 'react-bootstrap/InputGroup';
import Navbar from 'react-bootstrap/Navbar';
import Nav from 'react-bootstrap/Nav';
import Row from 'react-bootstrap/Row';
import Stack from 'react-bootstrap/Stack';
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

function InhMutRef() {
    return <span className="inherited-ref" title="inherited reference">&mut</span>
}

// Document all options. We split them into two categories: primary and
// secondary. Keys are `primary`, and either `question` or one entry per rule
// variant.
const prettyOptions = {
    match_constructor_through_ref: {
        primary: true,
        question: <>[p]: &[T]</>,
    },
    eat_inherited_ref_alone: {
        primary: true,
        question: <>&p: <InhRef/>[T]</>,
    },
    inherited_ref_on_ref: {
        primary: true,
        EatOuter: <>&p: <span style={{textDecoration: "underline"}} title="reference to consider"><InhRef/></span>&T</>,
        EatInner: <>&p: <InhRef/><span style={{textDecoration: "underline"}} title="reference to consider">&</span>T</>,
        EatBoth: <>&p: <span style={{textDecoration: "underline"}} title="reference to consider"><InhRef/>&</span>T</>,
    },
    ref_binding_on_inherited: {
        primary: true,
        ResetBindingMode: <>ref x: <InhRef/>T -&gt; x: &T</>,
        AllocTemporary: <>ref x: <InhRef/>T -&gt; x: &&T</>,
        Error: <>ref x: <InhRef/>T</>,
    },
    mut_binding_on_inherited: {
        primary: true,
        ResetBindingMode: <>mut x: <InhRef/>T -&gt; x: T</>,
        Keep: <>mut x: <InhRef/>T -&gt; x: &T</>,
        Error: <>mut x: <InhRef/>T</>,
    },
    fallback_to_outer: {
        primary: false,
        question: <>&mut p: <InhMutRef/> &T</>,
    },
    dont_eat_mut_inside_shared: {
        primary: false,
        question: <>not(&mut p: <InhRef/>&mut T)</>,
    },
    downgrade_mut_inside_shared: {
        primary: false,
        question: <><InhRef/>...&mut T -&gt; <InhRef/>...&T</>,
    },
    allow_ref_pat_on_ref_mut: {
        primary: false,
        question: <>&p: &mut T</>,
    },
}

export function CalculateWidth({ contents, setWidth }) {
    const spanRef = useRef(null);
    const style = {
        display: "inline-block",
        visibility: "hidden",
        position: "absolute",
    };
    // After first render, store the width.
    useEffect(() => setWidth(spanRef.current.offsetWidth), []);
    return <span style={style} ref={spanRef}>{contents}</span>
}

export function Preset({ options, setOptions }) {
    // const [width, setWidth] = useState(0);
    const active_bundle = options.get_bundle_name_js();
    const bundles = bundlesDoc.map(bundle => {
        return <Dropdown.Item
            key={bundle.name}
            title={bundle.doc}
            active={active_bundle == bundle.name}
            onClick={() => setOptions(bundle.options)}
        >
            {/* <CalculateWidth contents={bundle.name} setWidth={(w) => setWidth((old_w) => Math.max(old_w, w))}/> */}
            {bundle.name}
        </Dropdown.Item>
    });

    return <Nav.Item>
        <InputGroup>
            <InputGroup.Text>Preset</InputGroup.Text>
            <Dropdown>
                <Dropdown.Toggle>
                    {/* <span style={{display: "inline-block", width: width || null}}> */}
                        {active_bundle || "---"}
                    {/* </span> */}
                </Dropdown.Toggle>
                <Dropdown.Menu>{bundles}</Dropdown.Menu>
            </Dropdown>
        </InputGroup>
    </Nav.Item>
}

export default function SolverOptions({ options, setOptions }) {
    const setKey = (k, v) => setOptions(options.with_key(k, v));

    function make_option_elem(option) {
        const current_val = options.get_key(option.name);
        const prettyOption = prettyOptions[option.name] || null;
        const current_index = option.values.indexOf(current_val);
        const next_index = (current_index + 1) % option.values.length;
        const next_value = option.values[(current_index + 1) % option.values.length];
        const variant =
            current_val == "true" ? "outline-success" :
            (current_val == "false" || current_val == "Error") ? "outline-danger" :
            "outline-light";

        // Hide other possible values to ensure width doesn't change.
        const [optionsWidth, setOptionsWidth] = useState(0);
        const hiddenOptionsForWidth = option.values.filter((v) => prettyOption[v]).map((v) => {
            return <CalculateWidth key={v} contents={prettyOption[v]} setWidth={(w) => setOptionsWidth((old_w) => Math.max(old_w, w))}/>
        });

        return <Nav.Item key={option.name}>
            <Button
                variant={variant}
                title={option.doc}
                className="text-nowrap"
                onClick={(e) => { setKey(option.name, next_value); return false; }}
            >
                {hiddenOptionsForWidth}
                <span style={{display: "inline-block", width: optionsWidth || null}}>
                    {prettyOption[current_val] || prettyOption.question}
                </span>
            </Button>
        </Nav.Item>
    }

    const option_elems = optionsDoc.map(make_option_elem);

    return <Navbar expand="lg" className="bg-body-tertiary">
        <Container fluid>
            <Navbar.Brand>Options</Navbar.Brand>
            <Navbar.Toggle/>
            <Navbar.Collapse>
                <Nav className="me-auto">
                    <Preset {...{options, setOptions}}/>
                    {option_elems}
                </Nav>
            </Navbar.Collapse>
        </Container>
    </Navbar>
}
