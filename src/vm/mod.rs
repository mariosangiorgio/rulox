pub mod bytecode;
pub mod vm;

use frontend::scanner;
use user_interface::{RuloxImplementation, RunResult as UiRunResult};

pub struct RuloxVm {}

impl RuloxVm {
    pub fn new() -> RuloxVm {
        RuloxVm {}
    }
}

impl RuloxImplementation for RuloxVm {
    fn run(&mut self, source: &str) -> UiRunResult {
        let (tokens, errors) = scanner::scan(source);
        for token in tokens {
            println!("{:?}", token);
        }
        for error in errors {
            println!("{:?}", error);
        }
        UiRunResult::Error //TODO: implement
    }
}
