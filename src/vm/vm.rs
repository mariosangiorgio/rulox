use vm::bytecode::{Chunk};

pub enum RuntimeError {
}

struct Vm<'a> {
    chunk: &'a Chunk,
    program_counter: usize,
}

impl<'a> Vm<'a> {
    fn new(chunk: &'a Chunk) -> Vm<'a> {
        Vm {
            chunk: chunk,
            program_counter: 0,
        }
    }
    fn interpret(&mut self) -> Result<(), RuntimeError> {
        Ok(())
    }
}

pub fn interpret(chunk: & Chunk) -> Result<(), RuntimeError> {
    let mut vm = Vm::new(chunk);
    vm.interpret()
}
