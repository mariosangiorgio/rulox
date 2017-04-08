mod scanner;
mod ast;
mod pretty_printer;
mod parser;

extern crate itertools;

use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;
use pretty_printer::PrettyPrint;

#[derive(Debug)]
enum RunResult {
    Ok,
    IoError(String),
    InputError(Vec<InputError>),
    RuntimeError,
}

#[derive(Debug)]
enum InputError {
    ScannerError(scanner::ScannerError),
    ParserError(parser::ParseError),
}

fn run(source: &String) -> RunResult {
    // TODO: change scan and parse to return both what they managed
    // to do and a list of the errors they encountered.
    // This way we can report all the issues at once instead of
    // requiring lots of attempts
    let (tokens, scanner_errors) = scanner::scan(source);
    let mut errors: Vec<InputError> =
        scanner_errors.iter().map(|e| InputError::ScannerError(e.clone())).collect();
    match parser::parse(tokens) {
        Ok(expr) => {
            println!("{:?}", expr.pretty_print());
        }
        Err(parse_errors) => {
            for error in parse_errors {
                errors.push(InputError::ParserError(error))
            }
        }
    }
    if errors.is_empty() {
        RunResult::Ok
    } else {
        RunResult::InputError(errors)
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
