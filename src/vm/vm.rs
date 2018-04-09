use vm::bytecode::{disassemble_instruction, Chunk, OpCode, Value};
use std::io::{stdout, Error, LineWriter, Write};

#[derive(Debug)]
pub enum RuntimeError {
    TracingError(Error),
    StackUnderflow
}

struct Vm<'a> {
    chunk: &'a Chunk,
    program_counter: usize,
    stack: Vec<Value>,
}

impl<'a> Vm<'a> {
    fn new(chunk: &'a Chunk) -> Vm<'a> {
        Vm {
            chunk: chunk,
            program_counter: 0,
            stack: vec![],
        }
    }

    /// Interprets the next instruction.
    /// The execution of this function have some side effects including:
    ///  * update of the program counter to have it point to the next
    ///    instruction
    ///  * mutate the state of the stack
    ///
    /// This function true if there are other instructions left to execute
    /// or false if we're done interpreting the chunk.
    fn interpret_next(&mut self) -> Result<bool, RuntimeError> {
        self.program_counter += 1;
        match self.chunk.get(self.program_counter - 1) {
            OpCode::Return => {
                let value = self.stack.pop();
                println!{"{:?}", value};
                // Temporarily changed the meaning
                return Ok(false);
            }
            OpCode::Constant(offset) => self.stack.push(self.chunk.get_value(offset)),
            OpCode::Negate =>{
                if let Some(value) = self.stack.pop()
                {
                    self.stack.push(-value);
                }
                else{
                    return Err(RuntimeError::StackUnderflow)
                }
            }
        };
        Ok(true)
    }
    fn trace<T>(&mut self, out: &mut LineWriter<T>) -> Result<(), Error>
    where
        T: Write,
    {
        try!(write!(out, "Stack: "));
        for value in self.stack.iter() {
            try!(write!(out, "[ {} ]", value));
        }
        try!(writeln!(out));
        disassemble_instruction(&self.chunk.get(self.program_counter), self.chunk, out)
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
        try!{vm.trace(&mut writer).map_err(RuntimeError::TracingError)};
        try!{vm.interpret_next()}
    } {}
    Ok(())
}
