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
import Offcanvas from 'react-bootstrap/Offcanvas';
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

// How to render options. Keys are  either `question` (for boolean options) or
// one entry per rule variant.
const prettyOptions = {
    match_constructor_through_ref: {
        question: <>[p]: &[T]</>,
    },
    eat_inherited_ref_alone: {
        question: <>&p: <InhRef/>[T]</>,
    },
    inherited_ref_on_ref: {
        EatOuter: <>&p: <span style={{textDecoration: "underline"}} title="reference to consider"><InhRef/></span>&T</>,
        EatInner: <>&p: <InhRef/><span style={{textDecoration: "underline"}} title="reference to consider">&</span>T</>,
        EatBoth: <>&p: <span style={{textDecoration: "underline"}} title="reference to consider"><InhRef/>&</span>T</>,
    },
    ref_binding_on_inherited: {
        ResetBindingMode: <>ref x: <InhRef/>T -&gt; x: &T</>,
        AllocTemporary: <>ref x: <InhRef/>T -&gt; x: &&T</>,
        Error: <>ref x: <InhRef/>T</>,
    },
    mut_binding_on_inherited: {
        ResetBindingMode: <>mut x: <InhRef/>T -&gt; x: T</>,
        Keep: <>mut x: <InhRef/>T -&gt; x: &T</>,
        Error: <>mut x: <InhRef/>T</>,
    },
    fallback_to_outer: {
        question: <>&mut p: <InhMutRef/> &T</>,
    },
    eat_mut_inside_shared: {
        question: <>&mut p: <InhRef/>&mut T</>,
    },
    downgrade_mut_inside_shared: {
        question: <><InhRef/>...&mut T -&gt; <InhRef/>...&T</>,
    },
    allow_ref_pat_on_ref_mut: {
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
    const active_bundle = options.get_bundle_name_js();
    const bundles = bundlesDoc.map(bundle => {
        return <option
            key={bundle.name}
            value={bundle.name}
            title={bundle.doc}
        >
            {bundle.name}
        </option>
    });

    return <Form.Select
        value={active_bundle || "none"}
        onChange={(e) => {
            let bundle = e.target.value;
            if (bundle != "none") {
                setOptions(RuleOptions.from_bundle_name_js(bundle));
            }
        }}
    >
        <option value="none">---</option>
        {bundles}
    </Form.Select>
}

export function OptionElem({ option, options, setOptions, fullWidth }) {
    const setKey = (k, v) => setOptions(options.with_key(k, v));
    const current_val = options.get_key(option.name);
    const prettyOption = prettyOptions[option.name] || null;
    const irrelevant_options = options.irrelevant_options_js();

    const current_index = option.values.indexOf(current_val);
    const next_index = (current_index + 1) % option.values.length;
    const next_value = option.values[(current_index + 1) % option.values.length];

    const variant =
        current_val == "true" ? "outline-success" :
        (current_val == "false" || current_val == "Error") ? "outline-danger" :
        "outline-light";

    // Calculate width of all possible values to ensure width doesn't change when we click.
    const [optionsWidth, setOptionsWidth] = useState(0);
    const hiddenOptionsForWidth = option.values.filter((v) => prettyOption[v]).map((v) => {
        return <CalculateWidth
            key={v}
            contents={prettyOption[v]}
            setWidth={(w) => setOptionsWidth((old_w) => Math.max(old_w, w))}
        />
    });

    const style = {
        display: "inline-block",
        width: ((!fullWidth) && optionsWidth) || null
    };

    return <Button
        variant={variant}
        title={option.doc}
        className={"text-nowrap" + (fullWidth ? " w-100" : "")}
        disabled={irrelevant_options.includes(option.name)}
        onClick={(e) => { setKey(option.name, next_value); return false; }}
    >
        {hiddenOptionsForWidth}
        <span style={style}>
            {prettyOption[current_val] || prettyOption.question}
        </span>
    </Button>
}

export default function SolverOptions({ options, setOptions, title }) {
    const [navShow, setNavShow] = useState(false);
    const option_elems = optionsDoc.map((option) =>
        <Nav.Item key={option.name}>
            <OptionElem {...{option, options, setOptions, fullWidth: navShow}}/>
        </Nav.Item>
    );

    return <Navbar expand="lg" className="bg-body-tertiary">
        <Container fluid>
            <Navbar.Brand>{title}</Navbar.Brand>
            <Navbar.Toggle/>
            <Navbar.Offcanvas
                placement="end"
                scroll
                backdrop={false}
                className="w-auto"
                onEnter={() => setNavShow(true)}
                onExited={() => setNavShow(false)}
            >
                <Offcanvas.Header closeButton>
                    <Offcanvas.Title>{title}</Offcanvas.Title>
                </Offcanvas.Header>
                <Offcanvas.Body>
                    <Nav>
                        <Nav.Item>
                            <Preset {...{options, setOptions}}/>
                        </Nav.Item>
                        {option_elems}
                    </Nav>
                </Offcanvas.Body>
            </Navbar.Offcanvas>
        </Container>
    </Navbar>
}
