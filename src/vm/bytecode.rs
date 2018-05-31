use std::io::{Error, LineWriter, Write};

type Offset = usize;
pub type Value = f64;
type Line = usize;

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    Constant(Offset),
    Return,
    Negate,
    // Having a single binary opcode parametrized on its operand makes
    // the code cleaner.
    // Since constant already has an offset we're not making the
    // encoding worse. The extra space would have been allocated anyway.
    Binary(BinaryOp),
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug)]
pub struct Chunk {
    instructions: Vec<OpCode>,
    values: Vec<Value>,
    lines: Vec<Line>,
}

impl Chunk {
    pub fn new() -> Chunk {
        Chunk {
            instructions: Vec::new(),
            values: Vec::new(),
            lines: Vec::new(),
        }
    }

    pub fn get(&self, index: usize) -> OpCode {
        self.instructions[index]
    }

    pub fn get_value(&self, index: usize) -> Value {
        self.values[index]
    }

    /// Adds a new instruction to the chunk
    /// # Example
    /// ```
    /// use rulox::vm::bytecode::*;
    /// let mut chunk = Chunk::new();
    /// let line = 1;
    /// chunk.add_instruction(OpCode::Return, line);
    /// ```
    pub fn add_instruction(&mut self, instruction: OpCode, line: Line) -> () {
        self.instructions.push(instruction);
        self.lines.push(line);
    }

    pub fn instruction_count(&self) -> usize {
        self.instructions.len()
    }

    /// Constants live in a separate pool that needs to be pre-populated.
    /// Instructions that wants to use them need to reference them by
    /// their offset, which is returned by this function.
    /// # Example
    /// ```
    /// use rulox::vm::bytecode::*;
    /// let mut chunk = Chunk::new();
    /// let offset = chunk.add_constant(1.2);
    /// let line = 1;
    /// chunk.add_instruction(OpCode::Constant(offset), line);
    /// ```
    pub fn add_constant(&mut self, constant: Value) -> Offset {
        self.values.push(constant);
        self.values.len() - 1
    }

    pub fn values_count(&self) -> usize {
        self.values.len()
    }
}

pub fn disassemble_instruction<T>(
    instruction: &OpCode,
    chunk: &Chunk,
    out: &mut LineWriter<T>,
) -> Result<(), Error>
where
    T: Write,
{
    return match instruction {
        &OpCode::Return => writeln!(out, "OP_RETURN"),
        &OpCode::Constant(offset) => if offset >= chunk.values_count() {
            //TODO: this should probably return an error
            writeln!(out, "OP_CONSTANT {:4} 'ILLEGAL_ACCESS'", offset)
        } else {
            writeln!(
                out,
                "OP_CONSTANT {:4} '{:}'",
                offset,
                chunk.get_value(offset)
            )
        },
        &OpCode::Negate => writeln!(out, "OP_NEGATE"),
        &OpCode::Binary(ref operator) => match operator {
            &BinaryOp::Add => writeln!(out, "OP_ADD"),
            &BinaryOp::Subtract => writeln!(out, "OP_SUBTRACT"),
            &BinaryOp::Multiply => writeln!(out, "OP_MULTIPLY"),
            &BinaryOp::Divide => writeln!(out, "OP_DIVIDE"),
        },
    };
}

pub fn disassemble<T>(chunk: &Chunk, name: &str, out: &mut LineWriter<T>) -> Result<(), Error>
where
    T: Write,
{
    try!(writeln!(out, "== {} ==", name));
    let mut i = 0;
    let mut line = 0;
    for instruction in chunk.instructions.iter() {
        // Note that this is not printing offsets as the book does.
        // Using the OpCode enum all the opcodes have the same size.
        // It is not space-efficient, but for now it's fine
        try!(write!(out, "{:04}", i));
        if line == chunk.lines[i] {
            try!(write!(out, "   |"));
        } else {
            line = chunk.lines[i];
            try!(write!(out, "{:4}", line));
        }
        try!(write!(out, " "));
        i = i + 1;
        try!{disassemble_instruction(instruction, chunk, out)};
    }
    Ok(())
}

#[derive(Debug)]
pub enum RuntimeError {
    TracingError(Error),
    StackUnderflow,
    // The following out of bound errors have been found using
    // property based testing.
    // Unless I can statically determine that a chunk it is well-formed
    // they should be there, with a runtime penalty of bound checking
    // at every instruction
    InstructionOutOfBound,
    ValueOutOfBound,
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

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        if let Some(value) = self.stack.pop() {
            Ok(value)
        } else {
            Err(RuntimeError::StackUnderflow)
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
        if self.program_counter >= self.chunk.instruction_count() {
            return Err(RuntimeError::InstructionOutOfBound);
        }
        match self.chunk.get(self.program_counter - 1) {
            OpCode::Return => {
                let value = self.stack.pop();
                println!{"{:?}", value};
                // Temporarily changed the meaning
                return Ok(false);
            }
            OpCode::Constant(offset) => {
                if offset >= self.chunk.values_count() {
                    return Err(RuntimeError::ValueOutOfBound);
                }
                self.stack.push(self.chunk.get_value(offset))
            }
            OpCode::Negate => {
                let op = try!(self.pop());
                self.stack.push(-op);
            }
            OpCode::Binary(ref operator) => {
                // Note the order!
                // Op2 is the topmost element of the stack,
                // Op1 is the second topmost element
                let op2 = try!(self.pop());
                let op1 = try!(self.pop());
                self.stack.push(match operator {
                    &BinaryOp::Add => op1 + op2,
                    &BinaryOp::Subtract => op1 - op2,
                    &BinaryOp::Multiply => op1 * op2,
                    &BinaryOp::Divide => op1 / op2,
                })
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
        if self.program_counter < self.chunk.instruction_count() {
            disassemble_instruction(&self.chunk.get(self.program_counter), self.chunk, out)
        } else {
            //TODO: is this really ok?
            Ok(())
        }
    }
}

pub fn interpret(chunk: &Chunk) -> Result<(), RuntimeError> {
    let mut vm = Vm::new(chunk);
    while try!{vm.interpret_next()} {}
    Ok(())
}

pub fn trace<T>(chunk: &Chunk, writer: &mut LineWriter<T>) -> Result<(), RuntimeError>
where
    T: Write,
{
    let mut vm = Vm::new(chunk);
    while {
        try!{vm.trace(writer).map_err(RuntimeError::TracingError)};
        try!{vm.interpret_next()}
    } {}
    Ok(())
}

#[cfg(test)]
mod tests {
    use proptest::collection::*;
    use proptest::num::*;
    use proptest::prelude::*;
    use std::io::*;
    use vm::bytecode::*;

    fn arb_constants(max_constants: usize) -> VecStrategy<f64::Any> {
        prop::collection::vec(any::<Value>(), 0..max_constants)
    }

    fn arb_instruction(max_offset: usize) -> BoxedStrategy<OpCode> {
        prop_oneof![
            (0..max_offset).prop_map(OpCode::Constant),
            Just(OpCode::Return),
            Just(OpCode::Negate),
            prop_oneof![
                Just(BinaryOp::Add),
                Just(BinaryOp::Subtract),
                Just(BinaryOp::Multiply),
                Just(BinaryOp::Divide),
            ].prop_map(OpCode::Binary),
        ].boxed()
    }

    fn arb_instructions(
        max_offset: usize,
        max_instructions: usize,
    ) -> VecStrategy<BoxedStrategy<OpCode>> {
        prop::collection::vec(arb_instruction(max_offset), 0..max_instructions)
    }

    prop_compose!{
        fn arb_chunk(max_offset : usize, max_instructions : usize)
            (constants in arb_constants(max_offset),
             instructions in arb_instructions(max_offset, max_instructions)) -> Chunk{
            let mut chunk = Chunk::new();
            for constant in constants{
                let _offset = chunk.add_constant(constant);
            }
            let mut line = 0;
            for instruction in instructions{
                chunk.add_instruction(instruction, line);
                line = line + 1;
            }
            chunk
        }
    }

    proptest! {
    #[test]
    fn interpret_doesnt_crash(ref chunk in arb_chunk(10, 20)) {
        //TODO: this might not terminate
        let _ = interpret(chunk);
    }
    }

    proptest! {
    #[test]
    fn trace_doesnt_crash(ref chunk in arb_chunk(10, 20)) {
        let mut writer = LineWriter::new(sink());
        //TODO: this might not terminate
        let _ = trace(chunk, &mut writer);
    }
    }

    proptest! {
    #[test]
    fn disassemble_doesnt_crash(ref chunk in arb_chunk(10, 20)) {
        let mut writer = LineWriter::new(sink());
        let _ = disassemble(chunk, "Test", &mut writer);
    }
    }
}
