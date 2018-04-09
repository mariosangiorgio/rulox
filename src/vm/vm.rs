use vm::bytecode::{disassemble_instruction, Chunk, OpCode};
use std::io::{stdout, Error, LineWriter, Write};

pub enum RuntimeError {
    TracingError(Error),
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

    /// Interprets the next instruction.
    /// The execution of this function have some side effects including:
    ///  * update of the program counter to have it point to the next
    ///    instruction
    ///
    /// This function true if there are other instructions left to execute
    /// or false if we're done interpreting the chunk.
    fn interpret_next(&mut self) -> Result<bool, RuntimeError> {
        self.program_counter += 1;
        match self.chunk.get(self.program_counter - 1)
        {
            OpCode::Return =>
            // Temporarily changed the meaning
            return Ok(false),
            OpCode::Constant(offset) => format!("{}", self.chunk.get_value(offset)),
        };
        Ok(true)
    }
    fn trace<T>(&mut self, out: &mut LineWriter<T>) -> Result<(), RuntimeError>
    where
        T: Write,
    {
        disassemble_instruction(&self.chunk.get(self.program_counter), self.chunk, out)
            .map_err(RuntimeError::TracingError)
    }
}

pub fn interpret(chunk: &Chunk) -> Result<(), RuntimeError> {
    let mut vm = Vm::new(chunk);
    while try!{vm.interpret_next()} {}
    Ok(())
}

pub fn trace(chunk: &Chunk) -> Result<(), RuntimeError> {
    let mut vm = Vm::new(chunk);
    // Destination of the trace output
    let stdout = stdout();
    let handle = stdout.lock();
    let mut writer = LineWriter::new(handle);
    while {
        try!{vm.trace(&mut writer)};
        try!{vm.interpret_next()}
    } {}
    Ok(())
}
