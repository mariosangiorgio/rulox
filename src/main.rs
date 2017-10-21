mod scanner;
mod ast;
mod pretty_printer;
mod parser;
mod interpreter;
//mod lexical_scope_resolver;

extern crate itertools;

use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use pretty_printer::PrettyPrint;
use interpreter::{Interpreter, StatementInterpreter, Value, RuntimeError};
use ast::Statement;

#[derive(Debug)]
enum RunResult {
    Ok,
    IoError(String),
    InputError(Vec<InputError>),
    RuntimeError(RuntimeError),
}

#[derive(Debug)]
enum InputError {
    ScannerError(scanner::ScannerError),
    ParserError(parser::ParseError),
}

fn scan_and_parse(parser: &mut parser::Parser,
                  source: &str)
                  -> Result<Vec<ast::Statement>, Vec<InputError>> {
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

fn run(parser: &mut parser::Parser, interpreter: &mut Interpreter, source: &str) -> RunResult {
    match scan_and_parse(parser, source) {
        Ok(statements) => {
            for statement in statements {
                match interpreter.execute(&statement) {
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
            let mut parser = parser::Parser::new();
            let mut interpreter = StatementInterpreter::new();
            let mut source = String::new();
            match file.read_to_string(&mut source) {
                Err(_) => {
                    RunResult::IoError("Error reading file".into()) // TODO: add context
                }
                Ok(_) => run(&mut parser, &mut interpreter, &source),
            }
        }
    }
}

struct LoggingInterpreter {
    interpreter: StatementInterpreter,
}

impl LoggingInterpreter {
    fn new() -> LoggingInterpreter {
        LoggingInterpreter { interpreter: StatementInterpreter::new() }
    }
}

impl Interpreter for LoggingInterpreter {
    fn execute(&mut self, statement: &Statement) -> Result<Option<Value>, RuntimeError> {
        println!("{:?}", statement.pretty_print());
        let result = self.interpreter.execute(statement);
        {
            if let Ok(Some(ref value)) = result {
                println!("{:?}", value);
            };
        }
        result
    }
}

fn run_prompt() -> RunResult {
    println!("Rulox - A lox interpreter written in Rust");
    let mut parser = parser::Parser::new();
    let mut interpreter = LoggingInterpreter::new();
    let _ = io::stdout().flush(); //TODO: is this okay?
    loop {
        print!("> ");
        let _ = io::stdout().flush();
        let mut source = String::new();
        let _ = io::stdin().read_line(&mut source);
        // TODO: add a way to exit
        let result = run(&mut parser, &mut interpreter, &source);
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
