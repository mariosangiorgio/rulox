pub mod bytecode;
pub mod vm;

use user_interface::{RuloxImplementation, RunResult as UiRunResult};

pub struct RuloxVm {}

impl RuloxVm {
    pub fn new() -> RuloxVm {
        RuloxVm {}
    }
}

impl RuloxImplementation for RuloxVm {
    fn run(&mut self, source: &str) -> UiRunResult {
        UiRunResult::Error //TODO: implement
    }
}
