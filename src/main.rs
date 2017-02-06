use std::env;
use std::fs::File;
use std::io::prelude::*;

fn run(source: &String) -> i32 {
    panic!("Not implemented yet");
}

fn run_file(file_name: &String) -> i32 {
    match File::open(file_name) {
        Err(e) => {
            println!("Error opening {}", file_name);
            1
        }
        Ok(mut file) => {
            let mut source = String::new();
            match file.read_to_string(&mut source) {
                Err(e) => {
                    println!("Error reading {}", file_name);
                    1
                }
                Ok(_) => run(&source),
            }
        }
    }
}

fn run_prompt() -> i32 {
    panic!("Not implemented yet");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let exit_code = match args.len() {
        // The first argument is the program name
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => {
            println!("Usage: rulox [script]");
            1
        }
    };
    std::process::exit(exit_code)
}
