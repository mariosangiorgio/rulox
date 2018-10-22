extern crate rulox;

use rulox::user_interface::Runner;
use rulox::vm::LoxVm;
use std::env;

fn main() {
    let args: Vec<String> = env::args().collect();
    let mut runner = Runner::new(LoxVm::default());
    let exit_code = runner.run(&args);
    std::process::exit(exit_code)
}
