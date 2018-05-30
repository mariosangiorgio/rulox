pub mod bytecode;

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
        let mut errors = vec![];
        for scan_result in scanner::scan_into_iterator(source) {
            match scan_result {
                Ok(token_with_context) => println!("{:?}", token_with_context),
                Err(error) => errors.push(error), //TODO: wrap in generic error
            }
        }
        UiRunResult::Error //TODO: implement
    }
}

#[cfg(test)]
mod tests {
    use vm::RuloxVm;
    rulox_implementation_tests!{RuloxVm}
}