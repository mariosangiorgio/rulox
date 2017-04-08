mod scanner;
mod ast;
mod pretty_printer;
mod parser;
mod interpreter;

extern crate itertools;

use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use pretty_printer::PrettyPrint;
use interpreter::{Interpret, RuntimeError};

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

fn scan_and_parse(source: &String) -> Result<ast::Expr, Vec<InputError>> {
    let (tokens, scanner_errors) = scanner::scan(source);
    let mut errors: Vec<InputError> =
        scanner_errors.iter().map(|e| InputError::ScannerError(e.clone())).collect();
    match parser::parse(tokens) {
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

fn run(source: &String) -> RunResult {
    match scan_and_parse(source) {
        Ok(expr) => {
            println!("{:?}", expr.pretty_print());
            match expr.interpret() {
                Ok(value) => {
                    println!("{:?}", value);
                    RunResult::Ok
                }
                Err(e) => RunResult::RuntimeError(e),
            }
        }
        Err(e) => RunResult::InputError(e),
    }
}

fn run_file(file_name: &String) -> RunResult {
    match File::open(file_name) {
        Err(_) => {
            RunResult::IoError("Error opening file".into()) // TODO: add context
        }
        Ok(mut file) => {
            let mut source = String::new();
            match file.read_to_string(&mut source) {
                Err(_) => {
                    RunResult::IoError("Error reading file".into()) // TODO: add context
                }
                Ok(_) => run(&source),
            }
        }
    }
}

fn run_prompt() -> RunResult {
    println!("Rulox - A lox interpreter written in Rust");
    let _ = io::stdout().flush(); //TODO: is this okay?
    loop {
        print!("> ");
        let _ = io::stdout().flush();
        let mut source = String::new();
        let _ = io::stdin().read_line(&mut source);
        // TODO: add a way to exit
        let result = run(&source);
        match result {
            RunResult::Ok => (),
            _ => return result,
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
