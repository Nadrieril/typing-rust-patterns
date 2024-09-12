import init, { RuleOptions, style_from_name, trace_solver_js, display_rules_js } from "../../typing_rust_patterns/typing_rust_patterns.js";
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

function InhRef() {
    return <span className="inherited-ref" title="inherited reference">&</span>
}

const availableStyles = [
    { name: 'Sequent', display: <>inh, _ ⊢ p: <InhRef/>T</> },
    { name: 'SequentBindingMode', display: <>ref, _ ⊢ p: T</> },
];

// TODO: tab the options container to support bm-based Solver
// TODO: add second column for comparison
// TODO: encode current view in URL for sharing
export default function Solver() {
    const [style, setStyle] = useState('Sequent');
    const [options, setOptions] = useState(new RuleOptions());
    const [inputPattern, setInputPattern] = useState("[&x]: &mut [&T]");
    const [mode, setMode] = useState('typechecker');

    const solverSteps = useMemo(() => {
        const __html = trace_solver_js(inputPattern, options, style_from_name(style));
        return {__html}
    }, [inputPattern, options, style]);

    const rulesDisplay = useMemo(() => {
        const __html = display_rules_js(options, style_from_name(style));
        return {__html}
    }, [options, style]);

    const currentStyle = style;
    const styles = availableStyles.map(style => {
        return <Button variant="light"
            key={style.name}
            active={currentStyle == style.name}
            onClick={() => setStyle(style.name)}
        >
            {style.display}
        </Button>
    });

    return (
        <>
        <Container fluid>
            <Navbar expand="lg" sticky="top" className="bg-body-tertiary">
                <Container fluid>
                    <Navbar.Brand id="title"><span>T</span>yping <span>Ru</span>st <span>P</span>atterns</Navbar.Brand>
                    <Navbar.Toggle/>
                    <Navbar.Collapse>
                        <Nav className="me-auto">
                            <Nav.Link href="https://github.com/Nadrieril/typing-rust-patterns" target="_blank">See on Github</Nav.Link>
                        </Nav>
                        <Nav className="ms-auto">
                            <ButtonGroup title="predicate style">{styles}</ButtonGroup>
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
