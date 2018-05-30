use std::fs::File;
use std::io;
use std::io::prelude::*;

#[derive(Debug, PartialEq)]
pub enum RunResult {
    Ok,
    RuntimeError,
    InvalidProgram,
    IoError(String), //TODO: improve error reporting
}

pub trait RuloxImplementation {
    fn run(&mut self, source: &str) -> RunResult;
}

pub struct Runner<I: RuloxImplementation> {
    rulox: I,
}

impl<I: RuloxImplementation> Runner<I> {
    pub fn new(implementation: I) -> Runner<I> {
        Runner {
            rulox: implementation,
        }
    }
    pub fn run_file(&mut self, file_name: &str) -> RunResult {
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
                    Ok(_) => self.rulox.run(&source),
                }
            }
        }
    }

    fn run_prompt(&mut self) -> RunResult {
        println!("Rulox - A lox interpreter written in Rust");
        let _ = io::stdout().flush(); //TODO: is this okay?
        loop {
            print!("> ");
            let _ = io::stdout().flush();
            let mut source = String::new();
            let _ = io::stdin().read_line(&mut source);
            // TODO: add a way to exit
            let result = self.rulox.run(&source);
            match result {
                RunResult::Ok => (),
                _ => {
                    println!("{:?}", result);
                    let _ = io::stdout().flush();
                }
            }
        }
    }

    pub fn run(&mut self, args: &Vec<String>) -> i32 {
        let result = match args.len() {
            // The first argument is the program name
            1 => self.run_prompt(),
            2 => self.run_file(&args[1]),
            _ => {
                println!("Usage: rulox [script]");
                RunResult::Ok
            }
        };
        match result {
            RunResult::Ok => 0,
            _ => {
                println!("{:?}", result);
                1
            }
        }
    }
}
