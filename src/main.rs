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
enum Error {
    IoError(String),
    ScannerError(scanner::ScannerError),
    ParserError(parser::ParseError),
}

fn run(source: &String) -> Result<(), Vec<Error>> {
    // TODO: change scan and parse to return both what they managed
    // to do and a list of the errors they encountered.
    // This way we can report all the issues at once instead of
    // requiring lots of attempts
    let (tokens, scanner_errors) = scanner::scan(source);
    let errors : Vec<Error> = scanner_errors.iter().map(|e|Error::ScannerError(e.clone())).collect();
    if errors.is_empty() {
        match parser::parse(tokens) {
            Ok(expr) => {
                println!("{:?}", expr.pretty_print());
                Ok(())
            },
            Err(err) => Err(vec![Error::ParserError(err)]),
        }
    } else {
        Err(errors)
    }
}

fn run_file(file_name: &String) -> Result<(), Vec<Error>> {
    match File::open(file_name) {
        Err(_) => {
            Err(vec![Error::IoError("Error opening file".into())]) // TODO: add context
        }
        Ok(mut file) => {
            let mut source = String::new();
            match file.read_to_string(&mut source) {
                Err(_) => {
                    Err(vec![Error::IoError("Error reading file".into())]) // TODO: add context
                }
                Ok(_) => run(&source),
            }
        }
    }
}

fn run_prompt() -> Result<(), Vec<Error>> {
    println!("Rulox - A lox interpreter written in Rust");
    let _ = io::stdout().flush(); //TODO: is this okay?
    loop {
        print!("> ");
        let _ = io::stdout().flush();
        let mut source = String::new();
        let _ = io::stdin().read_line(&mut source);
        // TODO: add a way to exit
        try!(run(&source))
        // TODO: report syntax errors to the user
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
            Ok(())
        }
    };
    let exit_code = match result {
        Ok(_) => 0,
        Err(errors) => {
            for error in errors{
                println!("{:?}", error);
            }
            1
        }
    };
    std::process::exit(exit_code)
}
