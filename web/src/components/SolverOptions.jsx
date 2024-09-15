import init, { RuleSetJs } from "../../typing_rust_patterns/typing_rust_patterns.js";

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
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Row from 'react-bootstrap/Row';
import Stack from 'react-bootstrap/Stack';
import Table from 'react-bootstrap/Table';
import Tooltip from 'react-bootstrap/Tooltip';

await init({});

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
    const bundlesDoc = options.bundles_doc();
    const active_bundle_name = options.get_bundle_name();
    const active_bundle = bundlesDoc.filter(bundle => bundle.name == active_bundle_name).pop();
    const bundles = bundlesDoc.map(bundle => {
        return <option
            key={bundle.name}
            value={bundle.name}
            title={bundle.doc}
        >
            {bundle.name}
        </option>
    });

    return <OverlayTrigger placement="bottom" overlay={<Tooltip>{active_bundle ? active_bundle.doc : "This ruleset does not have a name"}</Tooltip>}>
        <Form.Select
            className="w-auto me-auto"
            value={active_bundle_name || "custom"}
            onChange={(e) => {
                let bundle = e.target.value;
                if (bundle != "custom") {
                    setOptions(options.with_bundle_name(bundle));
                }
            }}
        >
            <option value="custom">(custom)</option>
            {bundles}
        </Form.Select>
    </OverlayTrigger>
}

export function OptionElem({ option, options, setOptions, fullWidth }) {
    const setKey = (k, v) => setOptions(options.with_key(k, v));
    const prettyOption = prettyOptions[option.name] || {};
    const disabled = options.irrelevant_options().includes(option.name);

    const current_val = options.get_key(option.name);
    const current_index = option.values.findIndex((v) => v.name == current_val);
    const current_val_doc = option.values[current_index].doc;
    const next_index = (current_index + 1) % option.values.length;
    const next_value = option.values[next_index];

    const variant =
        disabled ? "outline-light" :
        (current_val == "false" || current_val == "Error") ? "outline-danger" :
        "outline-success";

    function textForValue(value_name) {
        return prettyOption[value_name]
            || prettyOption.question
            || ((value_name == "false" || value_name == "true") ? option.name : value_name)
    }

    // Calculate width of all possible values to ensure width doesn't change when we click.
    const [optionsWidth, setOptionsWidth] = useState(0);
    const hiddenOptionsForWidth = option.values.map((v) => {
        return <CalculateWidth
            key={v.name}
            contents={textForValue(v.name)}
            setWidth={(w) => setOptionsWidth((old_w) => Math.max(old_w, w))}
        />
    });

    const style = {
        display: "inline-block",
        width: ((!fullWidth) && optionsWidth) || null
    };

    return <OverlayTrigger placement="bottom" overlay={<Tooltip>{current_val_doc}</Tooltip>}>
        <Button
            variant={variant}
            className={"text-nowrap " + (fullWidth ? "w-100" : "")}
            disabled={disabled}
            onClick={(e) => { setKey(option.name, next_value.name); return false; }}
        >
            {hiddenOptionsForWidth}
            <span style={style}>
                {textForValue(current_val)}
            </span>
        </Button>
    </OverlayTrigger>
}

export default function SolverOptions({ options, setOptions, title }) {
    const [navShow, setNavShow] = useState(false);
    const [solverBtnWidth, setSolverBtnWidth] = useState(0);

    const select_solver = <OverlayTrigger
        placement="bottom"
        overlay={<Tooltip>{options.get_solver()
            ? "Nadri's type-based solver"
            : "TC's binding-mode-based solver"
        }</Tooltip>}
    >
        <Button
            variant="outline-light"
            onClick={() => setOptions(options.with_solver(!options.get_solver()))}
        >
            <CalculateWidth contents={"Ty"} setWidth={(w) => setSolverBtnWidth((old_w) => Math.max(old_w, w))} />
            <CalculateWidth contents={"BM"} setWidth={(w) => setSolverBtnWidth((old_w) => Math.max(old_w, w))} />
            <span style={{display: "inline-block", width: solverBtnWidth || null}}>
                {options.get_solver() ? "Ty" : "BM"}
            </span>
        </Button>
    </OverlayTrigger>;

    const option_elems = options
        .options_doc()
        .filter((opt) => opt.name != "simplify_deref_mut" && opt.name != "always_inspect_bm")
        .map((option) =>
        <Nav.Item key={option.name}>
            <OptionElem {...{option, options, setOptions, fullWidth: navShow}}/>
        </Nav.Item>
    );

    return <Navbar expand="xl" className="bg-body-tertiary">
        <Container fluid>
            <Stack direction="horizontal" gap={1}>
                {title ? <Navbar.Brand className="text-nowrap me-3">{title}</Navbar.Brand> : null}
                {select_solver}
                <Preset {...{options, setOptions}}/>
            </Stack>
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
                        <Stack direction={navShow ? "vertical" : "horizontal"} gap={1} className={navShow ? "" : "ms-1"}>
                            {option_elems}
                        </Stack>
                    </Nav>
                </Offcanvas.Body>
            </Navbar.Offcanvas>
        </Container>
    </Navbar>
}
