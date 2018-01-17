mod scanner;
mod ast;
mod pretty_printer;
mod parser;
mod interpreter;
mod lexical_scope_resolver;

extern crate itertools;

use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use interpreter::{RuntimeError, StatementInterpreter};
use parser::{ParseError, Parser};
use lexical_scope_resolver::{LexicalScopesResolutionError, ProgramLexicalScopesResolver};

#[derive(Debug)]
enum RunResult {
    Ok,
    IoError(String),
    InputError(Vec<InputError>),
    LexicalScopesResolutionError(Vec<LexicalScopesResolutionError>),
    RuntimeError(RuntimeError),
}

#[derive(Debug)]
enum InputError {
    ScannerError(scanner::ScannerError),
    ParserError(ParseError),
}

fn scan_and_parse(
    parser: &mut Parser,
    source: &str,
) -> Result<Vec<ast::Statement>, Vec<InputError>> {
    let (tokens, scanner_errors) = scanner::scan(source);
    let mut errors: Vec<InputError> = scanner_errors
        .iter()
        .map(|e| InputError::ScannerError(e.clone()))
        .collect();
    match parser.parse(&tokens) {
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

fn run(
    parser: &mut Parser,
    lexical_scope_resolver: &mut ProgramLexicalScopesResolver,
    interpreter: &mut StatementInterpreter,
    source: &str,
) -> RunResult {
    match scan_and_parse(parser, source) {
        Ok(statements) => {
            // Once we get the statements and their AST we run the following passes:
            // - lexical analysis
            // - actual interpretation
            let mut resolution_errors = vec![];
            for statement in statements.iter() {
                match lexical_scope_resolver.resolve(&statement) {
                    Ok(_) => (),
                    Err(e) => resolution_errors.push(e),
                }
            }
            if !resolution_errors.is_empty() {
                return RunResult::LexicalScopesResolutionError(resolution_errors);
            }
            for statement in statements.iter() {
                match interpreter.execute(lexical_scope_resolver, &statement) {
                    Ok(_) => (),
                    Err(e) => return RunResult::RuntimeError(e),
                }
            }
            RunResult::Ok
        }
        Err(e) => RunResult::InputError(e),
    }
}

fn run_file(file_name: &str) -> RunResult {
    match File::open(file_name) {
        Err(_) => {
            RunResult::IoError("Error opening file".into()) // TODO: add context
        }
        Ok(mut file) => {
            let mut parser = Parser::new();
            let mut lexical_scope_resolver = ProgramLexicalScopesResolver::new();
            let mut interpreter = StatementInterpreter::new(&mut parser.identifier_map);
            let mut source = String::new();
            match file.read_to_string(&mut source) {
                Err(_) => {
                    RunResult::IoError("Error reading file".into()) // TODO: add context
                }
                Ok(_) => run(
                    &mut parser,
                    &mut lexical_scope_resolver,
                    &mut interpreter,
                    &source,
                ),
            }
        }
    }
}

fn run_prompt() -> RunResult {
    println!("Rulox - A lox interpreter written in Rust");
    let mut parser = Parser::new();
    let mut lexical_scope_resolver = ProgramLexicalScopesResolver::new();
    let mut interpreter = StatementInterpreter::new(&mut parser.identifier_map);
    let _ = io::stdout().flush(); //TODO: is this okay?
    loop {
        print!("> ");
        let _ = io::stdout().flush();
        let mut source = String::new();
        let _ = io::stdin().read_line(&mut source);
        // TODO: add a way to exit
        let result = run(
            &mut parser,
            &mut lexical_scope_resolver,
            &mut interpreter,
            &source,
        );
        match result {
            RunResult::Ok => (),
            _ => {
                println!("{:?}", result);
                let _ = io::stdout().flush();
            }
        }
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let result = match args.len() {
        // The first argument is the program name
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => {
            println!("Usage: rulox [script]");
            RunResult::Ok
        }
    };
    let exit_code = match result {
        RunResult::Ok => 0,
        _ => {
            println!("{:?}", result);
            1
        }
    };
    std::process::exit(exit_code)
}
