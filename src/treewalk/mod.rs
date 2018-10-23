mod ast;
mod interpreter;
mod lexical_scope_resolver;
mod parser;
mod pretty_printer;

use self::ast::IdentifierMap;
use self::interpreter::{Environment, RuntimeError, StatementInterpreter};
use self::lexical_scope_resolver::{LexicalScopesResolutionError, LexicalScopesResolver};
use self::parser::{ParseError, Parser};
use frontend::scanner;
use user_interface::{LoxImplementation, RunError};

#[derive(Debug)]
enum LoxError {
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
    lexical_scope_resolver: LexicalScopesResolver,
    interpreter: StatementInterpreter,
}

impl Default for TreeWalkRuloxInterpreter {
    fn default() -> TreeWalkRuloxInterpreter {
        let mut identifier_map = IdentifierMap::new();
        let environment = Environment::new_with_natives(&mut identifier_map);
        let parser = Parser::new(identifier_map);
        TreeWalkRuloxInterpreter {
            parser,
            lexical_scope_resolver: LexicalScopesResolver::new(),
            interpreter: StatementInterpreter::new(environment),
        }
    }
}

impl TreeWalkRuloxInterpreter {
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

    fn run(&mut self, source: &str) -> Result<(), LoxError> {
        let statements = self.scan_and_parse(source).map_err(LoxError::InputError)?;
        let lexical_scope = self.lexical_scope_resolver
            .resolve_all(&statements)
            .map_err(LoxError::LexicalScopesResolutionError)?;
        for statement in &statements {
            self.interpreter
                .execute(&lexical_scope, &statement)
                .map_err(LoxError::RuntimeError)?;
        }
        Ok(())
    }
}

impl LoxImplementation for TreeWalkRuloxInterpreter {
    fn run(&mut self, source: &str) -> Result<(), RunError> {
        match self.run(source) {
            Ok(_) => Ok(()),
            //TODO: improve
            _ => Err(RunError::Error),
        }
    }
}

#[cfg(test)]
mod tests {
    use treewalk::*;

    proptest! {
    #[test]
    fn doesnt_crash(ref input in "\\PC*") {
        let mut lox_vm = TreeWalkRuloxInterpreter::default();
        lox_vm.run(input)
    }
    }
}
