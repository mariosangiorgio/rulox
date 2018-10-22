pub mod bytecode;
pub mod compiler;
pub mod interpreter;

use std::io::{stdout, LineWriter};
use user_interface::{LoxImplementation, RunError};

#[derive(Default)]
pub struct LoxVm {}

impl LoxImplementation for LoxVm {
    fn run(&mut self, source: &str) -> Result<(), RunError> {
        let chunk = compiler::compile(source).map_err(|_| RunError::Error)?;
        let stdout = stdout();
        let handle = stdout.lock();
        let mut writer = LineWriter::new(handle);
        bytecode::disassemble(&chunk, "Test", &mut writer).map_err(|_| RunError::Error)?;
        interpreter::trace(&chunk, &mut writer).map_err(|_| RunError::Error)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use vm::*;

    proptest! {
    #[test]
    #[ignore]
    fn doesnt_crash(ref input in "\\PC*") {
        let mut lox_vm = LoxVm::default();
        lox_vm.run(input)
    }
    }
}
