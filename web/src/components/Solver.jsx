import init, {
    RuleOptions,
    style_from_name,
    explain_predicate_js,
    trace_solver_js,
    display_rules_js,
    display_joint_rules_js,
    compare_rulesets_js,
} from "../../typing_rust_patterns/typing_rust_patterns.js";
import SolverOptions from './SolverOptions.jsx';

import { useEffect, useState, useMemo } from 'react';
import { useNavigate, useSearchParams } from "react-router-dom";
import Button from 'react-bootstrap/Button';
import ButtonGroup from 'react-bootstrap/ButtonGroup';
import Col from 'react-bootstrap/Col';
import Container from 'react-bootstrap/Container';
import Form from 'react-bootstrap/Form';
import InputGroup from 'react-bootstrap/InputGroup';
import Navbar from 'react-bootstrap/Navbar';
import Nav from 'react-bootstrap/Nav';
import Offcanvas from 'react-bootstrap/Offcanvas';
import OverlayTrigger from 'react-bootstrap/OverlayTrigger';
import Row from 'react-bootstrap/Row';
import Stack from 'react-bootstrap/Stack';
import Tab from 'react-bootstrap/Tab';
import Table from 'react-bootstrap/Table';
import Tabs from 'react-bootstrap/Tabs';
import Tooltip from 'react-bootstrap/Tooltip';

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
    {
        name: 'Sequent',
        display: <>inh, _ ⊢ p: <InhRef/>T</>,
        doc: "Track the type that the user observes and whether the outermost reference in the type is real or inherited"
    },
    {
        name: 'SequentBindingMode',
        display: <>ref, _ ⊢ p: T</>,
        doc: "Track the type of the matched place and the current binding mode"
    },
];

export function Help({show, setShow, style}) {
    return <>
        <Offcanvas placement="end" scroll show={show} onHide={() => setShow(false)}>
            <Offcanvas.Header closeButton>
                <Offcanvas.Title>Help</Offcanvas.Title>
            </Offcanvas.Header>
            <Offcanvas.Body>
                <p>
                Welcome to the interactive rust pattern typer!
                </p>
                <p>
                This website shows how a rust pattern may be typechecked, step by step.
                It is designed to experiment with possible rulesets and compare them.
                </p>
                <p>
                We represent the internal state of the typechecker as a predicate
                that looks like&nbsp;
                <span dangerouslySetInnerHTML={{__html: explain_predicate_js(style_from_name(style))}}/>
                </p>
                <p>
                Starting with the predicate that corresponds to your query, we
                update it step-by-step until we reach a success state (when the
                pattern is a binding).
                </p>
                <p>
                This tools has options for:
                </p>
                <ul>
                    <li>Typechecking a chosen pattern;</li>
                    <li>Displaying the rules used to typecheck;</li>
                    <li>Compare two rulesets exhaustively on all patterns and types under a given depth.</li>
                </ul>
            </Offcanvas.Body>
        </Offcanvas>
    </>
}

export function MainNavBar({compare, setCompare, style, setStyle}) {
    const navigate = useNavigate()
    const [searchParams, setSearchParams] = useSearchParams();
    function resetSearchParams() {
        const keys = Array.from(searchParams.keys());
        for (const key of keys) {
            searchParams.delete(key);
        }
        setSearchParams(searchParams);
        navigate(0); // Refresh
    }

    const [mainNavShow, setMainNavShow] = useState(false);
    const [helpShow, setHelpShow] = useState(false);

    const currentStyle = style;
    const styles = availableStyles.map(style => {
        return <OverlayTrigger key={style.name} placement="bottom" overlay={<Tooltip>{style.doc}</Tooltip>}>
            <Button
                active={currentStyle == style.name}
                onClick={() => setStyle(style.name)}
            >
                {style.display}
            </Button>
        </OverlayTrigger>
    });

    let title = <span id="title"><span>T</span>yping <span>Ru</span>st <span>P</span>atterns</span>;

    return <>
    <Navbar expand="md" className="bg-body-tertiary">
        <Container fluid>
            <OverlayTrigger placement="bottom" overlay={<Tooltip>Reset all settings</Tooltip>}>
                <Navbar.Brand onClick={resetSearchParams}>{title}</Navbar.Brand>
            </OverlayTrigger>
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
                    <Nav className="me-auto">
                        <Nav.Link
                             onClick={() => setHelpShow(true)}
                        >Help</Nav.Link>
                    </Nav>
                    <Nav className="ms-auto">
                        <Stack direction={mainNavShow ? "vertical" : "horizontal"} gap={1}>
                            <OverlayTrigger placement="bottom" overlay={<Tooltip>Compare two rulesets</Tooltip>}>
                                <Button
                                    active={compare}
                                    onClick={() => setCompare(!compare)}
                                >Compare</Button>
                            </OverlayTrigger>
                            <ButtonGroup vertical={mainNavShow}>{styles}</ButtonGroup>
                        </Stack>
                    </Nav>
                </Offcanvas.Body>
            </Navbar.Offcanvas>
        </Container>
    </Navbar>
    {/* Put the help outside the offcanvas because they can't be nested. */}
    <Help {...{show: helpShow, setShow: setHelpShow, style}}/>
    </>
}

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
    return <Table hover style={{tableLayout: "fixed"}}>
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

    return (
        <Container fluid>
            <div className="sticky-top">
                <MainNavBar {...{compare, setCompare, style, setStyle}}/>

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
                    <Table style={{tableLayout: "fixed"}}>
                        {compare ?
                            <thead><tr>
                                <td>Left</td>
                                <td>Right</td>
                            </tr></thead>
                        : null}
                        <tbody>
                            <tr>
                                <td><SolverSteps {...{inputQuery, options: optionsLeft, style}}/></td>
                                {compare ? <td><SolverSteps {...{inputQuery, options: optionsRight, style}}/></td> : null}
                            </tr>
                        </tbody>
                    </Table>
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
