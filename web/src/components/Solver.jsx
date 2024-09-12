import init, { RuleOptions, trace_solver_str } from "../../typing_rust_patterns/typing_rust_patterns.js";
import SolverOptions from './SolverOptions.jsx';

import { useState, useMemo } from 'react';
import Button from 'react-bootstrap/Button';
import ButtonGroup from 'react-bootstrap/ButtonGroup';
import Col from 'react-bootstrap/Col';
import Container from 'react-bootstrap/Container';
import Form from 'react-bootstrap/Form';
import InputGroup from 'react-bootstrap/InputGroup';
import Row from 'react-bootstrap/Row';
import Tab from 'react-bootstrap/Tab';
import Tabs from 'react-bootstrap/Tabs';
import Nav from 'react-bootstrap/Nav';
import Navbar from 'react-bootstrap/Navbar';

await init({});

// TODO: tab the options container to support bm-based Solver
// TODO: add second column for comparison
// TODO: encode current view in URL for sharing
export default function Solver() {
    const [options, setOptions] = useState(new RuleOptions());
    const [inputPattern, setInputPattern] = useState("[&x]: &mut [&T]");
    const [mode, setMode] = useState('typechecker');

    const solverSteps = useMemo(() => {
        const __html = trace_solver_str(inputPattern, options);
        return {__html}
    }, [inputPattern, options]);

    const rulesDisplay = useMemo(() => {
        const __html = options.display_rules_js();
        return {__html}
    }, [options]);

    return (
        <>
        <Container fluid>
            <Navbar expand="lg" className="bg-body-tertiary">
                <Container fluid>
                    <Navbar.Brand id="title"><span>T</span>yping <span>Ru</span>st <span>P</span>atterns</Navbar.Brand>
                    <Navbar.Toggle/>
                    <Navbar.Collapse>
                        <Nav className="me-auto">
                            <Nav.Link href="https://github.com/Nadrieril/typing-rust-patterns" target="_blank">See on Github</Nav.Link>
                        </Nav>
                    </Navbar.Collapse>
                </Container>
            </Navbar>
            <Row>
                <p id="foo">
                    Welcome to the interactive pattern typer!<br/>
                    Write <span className="monospace">`pattern: type`</span> in the input box to see it get typechecked.<br/>
                    Example: <span className="monospace">`&[ref x]: &[T]`</span>
                </p>
            </Row>
            <Row><SolverOptions options={options} setOptions={setOptions}/></Row>
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
                            value={inputPattern}
                            onChange={(e) => setInputPattern(e.target.value)}
                        />
                    </InputGroup>
                    <div className="monospace" dangerouslySetInnerHTML={solverSteps}/>
                </Tab>
                <Tab eventKey="rules" title="Rules">
                    <div className="monospace" dangerouslySetInnerHTML={rulesDisplay}/>
                </Tab>
            </Tabs>
            </Row>
        </Container>
        </>
    );
}
