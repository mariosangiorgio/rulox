use std::fs::File;
use std::io;
use std::io::prelude::*;

#[derive(Debug)]
pub enum RunError {
    Error,
    IoError(String), //TODO: improve error reporting
}

type RunResult = Result<(), RunError>;

pub trait LoxImplementation {
    fn run(&mut self, source: &str) -> RunResult;
}

pub struct Runner<I: LoxImplementation> {
    rulox: I,
}

impl<I: LoxImplementation> Runner<I> {
    pub fn new(implementation: I) -> Runner<I> {
        Runner {
            rulox: implementation,
        }
    }
    pub fn run_file(&mut self, file_name: &str) -> RunResult {
        let mut file =
            File::open(file_name).map_err(|_| RunError::IoError("Error opening file".into()))?; // TODO: add context
        let mut source = String::new();
        file.read_to_string(&mut source)
            .map_err(|_| RunError::IoError("Error reading file".into()))?;
        self.rulox.run(&source)
    }

    fn run_prompt(&mut self) -> Result<(), RunError> {
        println!("Rulox - A lox interpreter written in Rust");
        let _ = io::stdout().flush(); //TODO: is this okay?
        loop {
            print!("> ");
            io::stdout().flush().unwrap();
            let mut source = String::new();
            let _ = io::stdin().read_line(&mut source);
            // TODO: add a way to exit
            self.rulox.run(&source).unwrap_or_else(|error| {
                println!("{:?}", error);
                io::stdout().flush().unwrap()
            });
        }
    }

    pub fn run(&mut self, args: &[String]) -> i32 {
        let result = match args.len() {
            // The first argument is the program name
            1 => self.run_prompt(),
            2 => self.run_file(&args[1]),
            _ => {
                println!("Usage: rulox [script]");
                Ok(())
            }
        };
        match result {
            Ok(_) => 0,
            _ => {
                println!("{:?}", result);
                1
            }
        }
    }
}
