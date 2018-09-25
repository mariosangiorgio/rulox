use std::io::{Error, LineWriter, Write};

type Offset = usize;
#[derive(Debug, Clone, Copy)]
pub enum Value {
    Number(f64),
}
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
                "OP_CONSTANT {:4} '{:?}'",
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
