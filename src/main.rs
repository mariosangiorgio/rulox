use std::env;

fn run_file(file_name: &String) {
    panic!("Not implemented yet");
}

fn run_prompt() {
    panic!("Not implemented yet");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    match args.len() {
        // The first argument is the program name
        1 => run_prompt(),
        2 => run_file(&args[0]),
        _ => println!("Usage: rulox [script]"),
    }
}
