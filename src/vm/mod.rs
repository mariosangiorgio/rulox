pub mod bytecode;
pub mod compiler;
pub mod interpreter;

use std::io::{stdout, LineWriter};
use user_interface::{RuloxImplementation, RunResult as UiRunResult};

#[derive(Default)]
pub struct RuloxVm {}

impl RuloxImplementation for RuloxVm {
    fn run(&mut self, source: &str) -> UiRunResult {
        let chunk = compiler::compile(source).unwrap();
        let stdout = stdout();
        let handle = stdout.lock();
        let mut writer = LineWriter::new(handle);
        bytecode::disassemble(&chunk, "Test", &mut writer).unwrap();
        interpreter::trace(&chunk, &mut writer).unwrap();
        UiRunResult::Ok
    }
}

#[cfg(test)]
mod tests {
    use vm::*;

    proptest! {
    #[test]
    #[ignore]
    fn doesnt_crash(ref input in "\\PC*") {
        let mut ruloxvm = RuloxVm::new();
        ruloxvm.run(input)
    }
    }
}
