import init, {
    RuleSetJs,
    PredicateStyleJs,
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
import Dropdown from 'react-bootstrap/Dropdown';
import FloatingLabel from 'react-bootstrap/FloatingLabel';
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
function useStateInParams({searchParams, setSearchParams}, key, def, read = (x) => x, write = (x) => x) {
    function setSearchParam(k, v) {
        setSearchParams((params) => {
            params.set(k, v);
            return params
        });
    }

    const [val, setVal] = useState(() => read(searchParams.get(key)) || def);
    const setValAndParams = (v) => {
        setVal(v);
        setSearchParam(key, write(v));
    }
    return [val, setValAndParams]
}

function InhRef() {
    return <span className="inherited-ref" title="inherited reference">&</span>
}

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
                <span dangerouslySetInnerHTML={{__html: style.explain_predicate() }}/>
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
                <p>
                Two different solvers are available:&nbsp;
                <a href="https://github.com/Nadrieril" target="_blank">Nadrieril</a>'s&nbsp;
                <a href="https://github.com/Nadrieril/typing-rust-patterns" target="_blank">type-based solver</a>,
                and&nbsp;
                <a href="https://github.com/traviscross" target="_blank">TC</a>'s&nbsp;
                <a href="https://github.com/traviscross/match-ergonomics-formality" target="_blank">binding-mode-based solver</a>.
                </p>
            </Offcanvas.Body>
        </Offcanvas>
    </>
}

const availableStyles = ['Sequent', 'SequentBindingMode'];

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
    const styles = availableStyles.map(style_name => {
        let style = PredicateStyleJs.from_name(style_name);
        return <OverlayTrigger key={style_name} placement="bottom" overlay={<Tooltip>{style.doc()}</Tooltip>}>
            <Button
                active={currentStyle.to_name() == style_name}
                onClick={() => setStyle(style)}
                dangerouslySetInnerHTML={{__html: style.display_generic_predicate()}}
            ></Button>
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

export function SolverSteps({inputQuery, optionsLeft, optionsRight, compare, style}) {
    const [stepsLeft, resultLeft] = useMemo(() => {
        return optionsLeft.trace_solver(inputQuery, style);
    }, [inputQuery, optionsLeft, style]);
    const [stepsRight, resultRight] = useMemo(() => {
        return optionsRight.trace_solver(inputQuery, style);
    }, [inputQuery, optionsRight, style]);

    const resultStyle = {position: 'sticky', bottom: 0, fontWeight: 'normal'};

    return <Table style={{tableLayout: "fixed"}}>
        {compare ?
            <thead><tr>
                <td>Left</td>
                <td>Right</td>
            </tr></thead>
        : null}
        <tbody>
            <tr>
                <td><div className="monospace" dangerouslySetInnerHTML={{__html: stepsLeft}}/></td>
                {compare ? <td><div className="monospace" dangerouslySetInnerHTML={{__html: stepsRight}}/></td> : null}
            </tr>
            <tr>
                <th style={resultStyle} className="bg-body-tertiary">
                    Result:&nbsp;&nbsp;
                    <span className="monospace" dangerouslySetInnerHTML={{__html: resultLeft}}/>
                </th>
                {compare ?
                    <th style={resultStyle} className="bg-body-tertiary">
                        Result:&nbsp;&nbsp;
                        <span className="monospace" dangerouslySetInnerHTML={{__html: resultRight}}/>
                    </th>
                    : null
                }
            </tr>
        </tbody>
    </Table>
}

export function RulesDisplay({options, style}) {
    const rulesDisplay = useMemo(() => {
        return options.display_rules(style);
    }, [options, style]);

    const rows = rulesDisplay.map((rule, index) => {
        return <tr key={index}>
            <td><div className="monospace" dangerouslySetInnerHTML={{__html: rule}}/></td>
        </tr>
    });

    return <Table>
        <tbody>
            {rows}
        </tbody>
    </Table>
}

export function JointRulesDisplay({optionsLeft, optionsRight, style}) {
    const jointDisplay = useMemo(() => {
        return display_joint_rules_js(optionsLeft, optionsRight, style);
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

export function CompareDisplay({sp, optionsLeft, optionsRight, setInputQuery, setMode}) {
    const [patDepth, setPatDepth] = useStateInParams(sp, 'pat_d', 3, parseInt);
    const [tyDepth, setTyDepth] = useStateInParams(sp, 'ty_d', 4, parseInt);
    const [showCompare, setShowCompare] = useStateInParams(sp, 'do_cmp', false, (x) => x == 'true');
    const [compareDirection, setCompareDirection] = useStateInParams(sp, 'cmp_dir', 0, parseInt);
    // The input used in the last computation.
    const [compareInput, setCompareInput] = useState(null);
    // The output of the last computation.
    const [output, setOutput] = useState(null);

    // Set up the worker.
    const [worker, setWorker] = useState(null);
    useEffect(() => {
        const worker = new Worker(new URL("./worker.ts", import.meta.url), { type: "module" });
        worker.onmessage = function (event) {
            switch (event.data.type) {
                case "compare":
                    setOutput(event.data.output);
                    break;
                case "loaded":
                    if (showCompare) {
                        // If `showCompare` is true on init, this comes from the url so we
                        // should compute and show the output.
                        doCompare(worker);
                    }
                    break;
            }
        };
        setWorker(worker);

        // Clean up the worker when the component unmounts
        return () => {
            worker.terminate();
            // When going out of compare mode, set to false to avoid surprises.
            setShowCompare(false);
        };
    }, []);

    function doCompare(worker) {
        setShowCompare(true);
        setCompareInput({ optionsLeft, optionsRight, patDepth, tyDepth, compareDirection });
        setOutput(null);
        worker.postMessage({
            type: 'compare',
            optionsLeft: optionsLeft.encode(),
            optionsRight: optionsRight.encode(),
            patDepth,
            tyDepth,
            compareDirection,
        });
    }

    // Reset output if the options change.
    useEffect(() => {
        if (compareInput) {
            if (optionsLeft.eq(compareInput.optionsLeft)
                && optionsRight.eq(compareInput.optionsRight)
                && patDepth == compareInput.patDepth
                && tyDepth == compareInput.tyDepth
                && compareDirection == compareInput.compareDirection) {
                setShowCompare(true);
            } else {
                setShowCompare(false);
            }
        }
    }, [optionsLeft, optionsRight, patDepth, tyDepth, compareDirection, compareInput]);

    const rows = (output || []).map((diff, index) => {
        return <tr
            key={index}
            title="click to view this query in the typechecker"
            onClick={() => {
                setMode('typechecker');
                setInputQuery(diff.req);
            }}
        >
            <td><div className="monospace" dangerouslySetInnerHTML={{__html: diff.req}}/></td>
            <td><div className="monospace" dangerouslySetInnerHTML={{__html: diff.left}}/></td>
            <td><div className="monospace" dangerouslySetInnerHTML={{__html: diff.right}}/></td>
        </tr>
    });

    return <Stack gap={2}>
        <Form className="mb-3">
            <Stack direction="horizontal" gap={2} className="col-md-6">
                <FloatingLabel label="Pattern depth">
                    <Form.Control
                        type="number"
                        value={patDepth}
                        onChange={(e) => setPatDepth(e.target.value)}
                    />
                </FloatingLabel>
                <FloatingLabel label="Type depth">
                    <Form.Control
                        type="number"
                        value={tyDepth}
                        onChange={(e) => setTyDepth(e.target.value)}
                    />
                </FloatingLabel>
                <FloatingLabel label="Compare for">
                    <Form.Select
                        className="w-auto"
                        value={compareDirection}
                        onChange={(e) => setCompareDirection(e.target.value)}
                    >
                        <option value={0}>Equality</option>
                        <option value={-1}>Left-to-right compatibility</option>
                        <option value={1}>Right-to-left compatibility</option>
                    </Form.Select>
                </FloatingLabel>
                <Button onClick={() => doCompare(worker)}>Compare</Button>
            </Stack>
        </Form>
        {!showCompare
            ? null
            : !output
            ? <>Working...</>
            : !output.length
            ? (compareDirection == 0
                ? <>No differences</>
                : compareDirection < 0
                ? <>Left appears forward-compatible with Right</>
                : <>Right appears forward-compatible with Left</>
            )
            : <Table bordered hover>
                <thead><tr>
                    <td>Query</td>
                    <td>Left</td>
                    <td>Right</td>
                </tr></thead>
                <tbody>
                    {rows}
                </tbody>
            </Table>
        }
    </Stack>
}

export default function Solver() {
    // Decoding function that simply checks that the value is in the given array.
    function validateIn(allowed) {
        return (v) => allowed.includes(v) ? v : null;
    }

    const [searchParams, setSearchParams] = useSearchParams();
    const sp = {searchParams, setSearchParams};
    const [compare, setCompare] = useStateInParams(sp, 'compare', false, (x) => x == 'true');
    const defaultStyle = PredicateStyleJs.from_name('Sequent');
    const [style, setStyle] = useStateInParams(sp, 'style', defaultStyle, PredicateStyleJs.from_name, (style) => style.to_name());
    const [optionsLeft, setOptionsLeft] = useStateInParams(sp, 'opts1', RuleSetJs.from_bundle_name('nadri', 'stable'), RuleSetJs.decode, (o) => o.encode());
    const [optionsRight, setOptionsRight] = useStateInParams(sp, 'opts2', RuleSetJs.from_bundle_name('rfc3627', 'rfc3627'), RuleSetJs.decode, (o) => o.encode());
    const [inputQuery, setInputQuery] = useStateInParams(sp, 'q', "[&x]: &mut [&T]");
    const [mode, setMode] = useStateInParams(sp, 'mode', 'typechecker', validateIn(['typechecker', 'rules', 'compare']));

    return (
        <>
            <div className="sticky-top">
                <MainNavBar {...{compare, setCompare, style, setStyle}}/>
                <SolverOptions options={optionsLeft} setOptions={setOptionsLeft} title={compare ? <>Left&nbsp;&nbsp;&nbsp;</> : null}/>
                {compare ? <SolverOptions options={optionsRight} setOptions={setOptionsRight} title="Right"/> : null}
            </div>
            <Row>
            <Tabs
                activeKey={(!compare && mode =='compare') ? 'typechecker' : mode}
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
                    <SolverSteps {...{inputQuery, optionsLeft, optionsRight, compare, style}}/>
                </Tab>
                <Tab eventKey="rules" title="Rules">
                    {
                        !optionsLeft.get_solver() || (compare && !optionsRight.get_solver()) ?
                        <>The binding-mode-based solver does not support listing its rules</>
                        : compare
                        ? <JointRulesDisplay {...{optionsLeft, optionsRight, style}}/>
                        : <RulesDisplay {...{options: optionsLeft, style}}/>
                    }
                </Tab>
               {compare ?
                   <Tab eventKey="compare" title="Compare">
                       <CompareDisplay {...{sp, optionsLeft, optionsRight, setInputQuery, setMode}}/>
                   </Tab>
               : null}
            </Tabs>
            </Row>
        </>
    );
}
