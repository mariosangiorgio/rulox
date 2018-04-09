use vm::bytecode::{Chunk, OpCode};

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
        loop {
            self.program_counter += 1;
            match self.chunk.get(self.program_counter -1)
        {
          OpCode::Return =>
            // Temporarily changed the meaning
            return Ok(()),
          OpCode::Constant(offset) => format!("{}", self.chunk.get_value(offset)),
        };
        }
    }
}

pub fn interpret(chunk: &Chunk) -> Result<(), RuntimeError> {
    let mut vm = Vm::new(chunk);
    vm.interpret()
}
