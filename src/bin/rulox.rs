use std::env;

extern crate rulox;

fn main() {
    let args: Vec<String> = env::args().collect();
    let result = match args.len() {
        // The first argument is the program name
        1 => rulox::run_prompt(),
        2 => rulox::run_file(&args[1]),
        _ => {
            println!("Usage: rulox [script]");
            rulox::RunResult::Ok
        }
    };
    let exit_code = match result {
        rulox::RunResult::Ok => 0,
        _ => {
            println!("{:?}", result);
            1
        }
    };
    std::process::exit(exit_code)
}
