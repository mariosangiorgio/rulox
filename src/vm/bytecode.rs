use std::io::{Error, LineWriter, Write};

type Offset = usize;
/// Subset of values that can be initialised when a chunk is created.
/// They will be turned into proper values when the VM accesses them.
#[derive(Debug)]
pub enum Constant {
    Number(f64),
    Bool(bool),
    Nil,
    String(String),
}
type Line = usize;

#[derive(Debug, Clone, Copy)]
pub enum OpCode {
    Constant(Offset),
    Return,
    Negate,
    Not,
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
    Equals,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Or,
    And,
}

#[derive(Debug, Default)]
pub struct Chunk {
    instructions: Vec<OpCode>,
    values: Vec<Constant>,
    lines: Vec<Line>,
}

impl Chunk {
    pub fn get(&self, index: usize) -> OpCode {
        self.instructions[index]
    }

    pub fn get_value(&self, index: usize) -> &Constant {
        &self.values[index]
    }

    /// Adds a new instruction to the chunk
    /// # Example
    /// ```
    /// use rulox::vm::bytecode::*;
    /// let mut chunk = Chunk::default();
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
    /// let mut chunk = Chunk::default();
    /// let offset = chunk.add_constant(Constant::Number(1.2));
    /// let line = 1;
    /// chunk.add_instruction(OpCode::Constant(offset), line);
    /// ```
    pub fn add_constant(&mut self, constant: Constant) -> Offset {
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
    match *instruction {
        OpCode::Return => writeln!(out, "OP_RETURN"),
        OpCode::Constant(offset) => if offset >= chunk.values_count() {
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
        OpCode::Not => writeln!(out, "OP_NOT"),
        OpCode::Negate => writeln!(out, "OP_NEGATE"),
        OpCode::Binary(ref operator) => match *operator {
            BinaryOp::Add => writeln!(out, "OP_ADD"),
            BinaryOp::Subtract => writeln!(out, "OP_SUBTRACT"),
            BinaryOp::Multiply => writeln!(out, "OP_MULTIPLY"),
            BinaryOp::Divide => writeln!(out, "OP_DIVIDE"),
            BinaryOp::Equals => writeln!(out, "OP_EQUALS"),
            BinaryOp::NotEqual => writeln!(out, "OP_NOT_EQUAL"),
            BinaryOp::Greater => writeln!(out, "OP_GREATER"),
            BinaryOp::GreaterEqual => writeln!(out, "OP_GREATER_EQUAL"),
            BinaryOp::Less => writeln!(out, "OP_LESS"),
            BinaryOp::LessEqual => writeln!(out, "OP_LESS_EQUAL"),
            BinaryOp::Or => writeln!(out, "OP_OR"),
            BinaryOp::And => writeln!(out, "OP_AND"),
        },
    }
}

pub fn disassemble<T>(chunk: &Chunk, name: &str, out: &mut LineWriter<T>) -> Result<(), Error>
where
    T: Write,
{
    try!(writeln!(out, "== {} ==", name));
    let mut line = 0;
    for (i, instruction) in chunk.instructions.iter().enumerate() {
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
        try!{disassemble_instruction(instruction, chunk, out)};
    }
    Ok(())
}
