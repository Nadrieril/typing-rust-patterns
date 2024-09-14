import init, {
    RuleOptions,
    style_from_name,
    trace_solver_js,
    display_rules_js,
    display_joint_rules_js,
    compare_rulesets_js,
} from "../../typing_rust_patterns/typing_rust_patterns.js";
import SolverOptions from './SolverOptions.jsx';

import { useEffect, useState, useMemo } from 'react';
import { useSearchParams } from "react-router-dom";
import Button from 'react-bootstrap/Button';
import ButtonGroup from 'react-bootstrap/ButtonGroup';
import Col from 'react-bootstrap/Col';
import Container from 'react-bootstrap/Container';
import Form from 'react-bootstrap/Form';
import InputGroup from 'react-bootstrap/InputGroup';
import Navbar from 'react-bootstrap/Navbar';
import Nav from 'react-bootstrap/Nav';
import Offcanvas from 'react-bootstrap/Offcanvas';
import Row from 'react-bootstrap/Row';
import Stack from 'react-bootstrap/Stack';
import Tab from 'react-bootstrap/Tab';
import Table from 'react-bootstrap/Table';
import Tabs from 'react-bootstrap/Tabs';

await init({});

// Like `useState`, but mirror the value in the search parameters.
function useStateInParams(key, def, read = (x) => x, write = (x) => x) {
    const [searchParams, setSearchParams] = useSearchParams();
    function setSearchParam(k, v) {
        setSearchParams((params) => {
            params.set(k, v);
            return params
        });
    }

    let start_val = read(searchParams.get(key)) || def;

    const [val, setVal] = useState(start_val);
    const setValAndParams = (v) => {
        setVal(v);
        setSearchParam(key, write(v));
    }
    return [val, setValAndParams]
}

function InhRef() {
    return <span className="inherited-ref" title="inherited reference">&</span>
}

const availableStyles = [
    { name: 'Sequent', display: <>inh, _ ⊢ p: <InhRef/>T</> },
    { name: 'SequentBindingMode', display: <>ref, _ ⊢ p: T</> },
];

export function SolverSteps({inputQuery, options, style}) {
    const solverSteps = useMemo(() => {
        const __html = trace_solver_js(inputQuery, options, style_from_name(style));
        return {__html}
    }, [inputQuery, options, style]);

    return <div className="monospace" dangerouslySetInnerHTML={solverSteps}/>
}

export function RulesDisplay({options, style}) {
    const rulesDisplay = useMemo(() => {
        const __html = display_rules_js(options, style_from_name(style));
        return {__html}
    }, [options, style]);

    return <div className="monospace" dangerouslySetInnerHTML={rulesDisplay}/>
}

export function JointRulesDisplay({optionsLeft, optionsRight, style}) {
    const jointDisplay = useMemo(() => {
        return display_joint_rules_js(optionsLeft, optionsRight, style_from_name(style));
    }, [optionsLeft, optionsRight, style]);

    const rows = jointDisplay.map((joint, index) => {
        return <tr key={index}>
            <td><div className="monospace" dangerouslySetInnerHTML={{__html: joint.left}}/></td>
            <td><div className="monospace" dangerouslySetInnerHTML={{__html: joint.right}}/></td>
        </tr>
    });
    return <Table hover>
        <thead><tr>
            <td>Left</td>
            <td>Right</td>
        </tr></thead>
        <tbody>
            {rows}
        </tbody>
    </Table>
}

export function CompareDisplay({optionsLeft, optionsRight}) {
    const [patDepth, setPatDepth] = useStateInParams('pat_d', 3, parseInt);
    const [tyDepth, setTyDepth] = useStateInParams('ty_d', 4, parseInt);
    const [output, setOutput] = useState([]);

    // Reset output if the options change.
    useEffect(() => {
        setOutput([])
    }, [optionsLeft, optionsRight]);

    const rows = output.map((diff, index) => {
        return <tr key={index}>
            <td><div className="monospace" dangerouslySetInnerHTML={{__html: diff.req}}/></td>
            <td><div className="monospace" dangerouslySetInnerHTML={{__html: diff.left}}/></td>
            <td><div className="monospace" dangerouslySetInnerHTML={{__html: diff.right}}/></td>
        </tr>
    });
    return <Stack gap={2}>
        <Form className="mb-3">
            <Stack direction="horizontal" gap={2} className="col-md-4">
                <InputGroup>
                    <InputGroup.Text>Pattern depth</InputGroup.Text>
                    <Form.Control
                        type="number"
                        value={patDepth}
                        onChange={(e) => setPatDepth(e.target.value)}
                    />
                </InputGroup>
                <InputGroup>
                    <InputGroup.Text>Type depth</InputGroup.Text>
                    <Form.Control
                        type="number"
                        value={tyDepth}
                        onChange={(e) => setTyDepth(e.target.value)}
                    />
                </InputGroup>
                <Button
                    onClick={() => setOutput(compare_rulesets_js(optionsLeft, optionsRight, patDepth, tyDepth))}
                >
                    Compare
                </Button>
            </Stack>
        </Form>
        {output.length ?
            <Table bordered hover>
                <thead><tr>
                    <td>Query</td>
                    <td>Left</td>
                    <td>Right</td>
                </tr></thead>
                <tbody>
                    {rows}
                </tbody>
            </Table>
            : null
        }
    </Stack>
}

// TODO: tab the options container to support bm-based Solver
// TODO: make hover info more visible
// TODO: add offcanvas with predicate and rules explanations
export default function Solver() {
    // Decoding function that simply checks that the value is in the given array.
    function validateIn(allowed) {
        return (v) => allowed.includes(v) ? v : null;
    }

    const [compare, setCompare] = useStateInParams('compare', false, (x) => x == 'true');
    const [style, setStyle] = useStateInParams('style', 'Sequent', validateIn(['Sequent', 'SequentBindingMode', 'Expression']));
    const [optionsLeft, setOptionsLeft] = useStateInParams('opts1', RuleOptions.from_bundle_name_js('nadri'), RuleOptions.decode, (o) => o.encode());
    const [optionsRight, setOptionsRight] = useStateInParams('opts2', RuleOptions.from_bundle_name_js('rfc3627'), RuleOptions.decode, (o) => o.encode());
    const [inputQuery, setInputQuery] = useStateInParams('q', "[&x]: &mut [&T]");
    const [mode, setMode] = useStateInParams('mode', 'typechecker', validateIn(['typechecker', 'rules', 'compare']));

    const currentStyle = style;
    const styles = availableStyles.map(style => {
        return <Button
            key={style.name}
            active={currentStyle == style.name}
            onClick={() => setStyle(style.name)}
        >
            {style.display}
        </Button>
    });

    let title = <span id="title"><span>T</span>yping <span>Ru</span>st <span>P</span>atterns</span>;
    const [mainNavShow, setMainNavShow] = useState(false);

    return (
        <Container fluid>
            <div className="sticky-top">
                <Navbar expand="md" className="bg-body-tertiary">
                    <Container fluid>
                        <Navbar.Brand>{title}</Navbar.Brand>
                        <Nav className="me-auto">
                            <Nav.Link href="https://github.com/Nadrieril/typing-rust-patterns" target="_blank">See on Github</Nav.Link>
                        </Nav>
                        <Navbar.Toggle/>
                        <Navbar.Offcanvas
                            placement="end"
                            scroll
                            backdrop={false}
                            className="w-auto"
                            onEnter={() => setMainNavShow(true)}
                            onExited={() => setMainNavShow(false)}
                        >
                            <Offcanvas.Header closeButton>
                                <Offcanvas.Title>General options</Offcanvas.Title>
                            </Offcanvas.Header>
                            <Offcanvas.Body>
                                <Nav className="ms-auto">
                                    <Stack direction={mainNavShow ? "vertical" : "horizontal"} gap={1}>
                                        <Button
                                            active={compare}
                                            onClick={() => setCompare(!compare)}
                                        >Compare</Button>
                                        <ButtonGroup title="predicate style" vertical={mainNavShow}>{styles}</ButtonGroup>
                                    </Stack>
                                </Nav>
                            </Offcanvas.Body>
                        </Navbar.Offcanvas>
                    </Container>
                </Navbar>

                {/* Keep the options around and undisplayed to avoid flickering
                    when we mount/unmount them and they have to update button
                    widths after the first render. */}
                <div style={{display: !compare ? 'none' : null}}>
                    <SolverOptions options={optionsLeft} setOptions={setOptionsLeft} title=<>Left&nbsp;&nbsp;&nbsp;</>/>
                    <SolverOptions options={optionsRight} setOptions={setOptionsRight} title="Right"/>
                </div>
                <div style={{display: compare ? 'none' : null}}>
                    <SolverOptions options={optionsLeft} setOptions={setOptionsLeft} title="Options"/>
                </div>

            </div>
            <Row>
                <p id="foo">
                    Welcome to the interactive pattern typer!<br/>
                    Write <span className="monospace">`pattern: type`</span> in the input box to see it get typechecked.<br/>
                    Example: <span className="monospace">`&[ref x]: &[T]`</span>
                </p>
            </Row>
            <Row>
            <Tabs
                activeKey={mode}
                onSelect={(k) => setMode(k)}
                transition={false}
                className="mb-3"
                fill
            >
                <Tab eventKey="typechecker" title="Typechecker">
                    <InputGroup className="mb-3">
                        <InputGroup.Text>Query</InputGroup.Text>
                        <Form.Control
                            placeholder="[&x]: &mut [&T]"
                            value={inputQuery}
                            onChange={(e) => setInputQuery(e.target.value)}
                        />
                    </InputGroup>
                    <Container fluid>
                    <Row>
                    <Col><SolverSteps {...{inputQuery, options: optionsLeft, style}}/></Col>
                    {compare ? <Col><SolverSteps {...{inputQuery, options: optionsRight, style}}/></Col> : null}
                    </Row>
                    </Container>
                </Tab>
                <Tab eventKey="rules" title="Rules">
                    {compare
                        ? <JointRulesDisplay {...{optionsLeft, optionsRight, style}}/>
                        : <RulesDisplay {...{options: optionsLeft, style}}/>
                    }
                </Tab>
               {compare ?
                   <Tab eventKey="compare" title="Compare">
                       <CompareDisplay {...{optionsLeft, optionsRight}}/>
                   </Tab>
               : null}
            </Tabs>
            </Row>
        </Container>
    );
}
