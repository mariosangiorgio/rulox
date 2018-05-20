use rulox::treewalk::TreeWalkRuloxInterpreter;
use rulox::user_interface::Runner;
use std::env;

extern crate rulox;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut runner = Runner::new(TreeWalkRuloxInterpreter::new());
    let exit_code = runner.run(&args);
    std::process::exit(exit_code)
}
