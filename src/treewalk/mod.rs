mod ast;
mod interpreter;
mod lexical_scope_resolver;
mod parser;
mod pretty_printer;

use self::interpreter::{Environment, RuntimeError, StatementInterpreter};
use self::lexical_scope_resolver::{LexicalScopesResolutionError, ProgramLexicalScopesResolver};
use self::parser::{ParseError, Parser};
use user_interface::{RuloxImplementation, RunResult as UiRunResult};
use frontend::scanner;

#[derive(Debug)]
enum RunResult {
    Ok,
    InputError(Vec<InputError>),
    LexicalScopesResolutionError(Vec<LexicalScopesResolutionError>),
    RuntimeError(RuntimeError),
}

#[derive(Debug)]
enum InputError {
    ScannerError(scanner::ScannerError),
    ParserError(ParseError),
}

pub struct TreeWalkRuloxInterpreter {
    parser: Parser,
    lexical_scope_resolver: ProgramLexicalScopesResolver,
    interpreter: StatementInterpreter,
}

impl TreeWalkRuloxInterpreter {
    pub fn new() -> TreeWalkRuloxInterpreter {
        let mut parser = Parser::new();
        let environment = Environment::new_with_natives(&mut parser.identifier_map);
        TreeWalkRuloxInterpreter {
            parser: parser,
            lexical_scope_resolver: ProgramLexicalScopesResolver::new(),
            interpreter: StatementInterpreter::new(environment),
        }
    }

    fn scan_and_parse(&mut self, source: &str) -> Result<Vec<ast::Statement>, Vec<InputError>> {
        let (tokens, scanner_errors) = scanner::scan(source);
        let mut errors: Vec<InputError> = scanner_errors
            .iter()
            .map(|e| InputError::ScannerError(e.clone()))
            .collect();
        match self.parser.parse(&tokens) {
            Ok(expr) => {
                if errors.is_empty() {
                    Ok(expr)
                } else {
                    Err(errors)
                }
            }
            Err(parse_errors) => {
                for error in parse_errors {
                    errors.push(InputError::ParserError(error))
                }
                Err(errors)
            }
        }
    }

    fn run(&mut self, source: &str) -> RunResult {
        match self.scan_and_parse(source) {
            Ok(statements) => {
                // Once we get the statements and their AST we run the following passes:
                // - lexical analysis
                // - actual interpretation
                let mut resolution_errors = vec![];
                for statement in statements.iter() {
                    match self.lexical_scope_resolver.resolve(&statement) {
                        Ok(_) => (),
                        Err(e) => resolution_errors.push(e),
                    }
                }
                if !resolution_errors.is_empty() {
                    return RunResult::LexicalScopesResolutionError(resolution_errors);
                }
                for statement in statements.iter() {
                    match self.interpreter
                        .execute(&self.lexical_scope_resolver, &statement)
                    {
                        Ok(_) => (),
                        Err(e) => return RunResult::RuntimeError(e),
                    }
                }
                RunResult::Ok
            }
            Err(e) => RunResult::InputError(e),
        }
    }
}

impl RuloxImplementation for TreeWalkRuloxInterpreter {
    fn run(&mut self, source: &str) -> UiRunResult {
        match self.run(source) {
            RunResult::Ok => UiRunResult::Ok,
            //TODO: improve
            _ => UiRunResult::Error,
        }
    }
}
