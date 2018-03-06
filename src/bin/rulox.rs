use std::env;

extern crate rulox;

fn main() {
    let args: Vec<String> = env::args().collect();
    let result = match args.len() {
        // The first argument is the program name
        1 => rulox::treewalk::run_prompt(),
        2 => rulox::treewalk::run_file(&args[1]),
        _ => {
            println!("Usage: rulox [script]");
            rulox::treewalk::RunResult::Ok
        }
    };
    let exit_code = match result {
        rulox::treewalk::RunResult::Ok => 0,
        _ => {
            println!("{:?}", result);
            1
        }
    };
    std::process::exit(exit_code)
}
