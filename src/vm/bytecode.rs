use std::io::{Error, LineWriter, Write};

type Offset = usize;
type Value = f64;
type Line = usize;

pub enum OpCode {
    Constant(Offset),
    Return,
}

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

    pub fn disassemble<T>(&self, name: &str, out: &mut LineWriter<T>) -> Result<(), Error>
    where
        T: Write,
    {
        try!(writeln!(out, "== {} ==", name));
        let mut i = 0;
        let mut line = 0;
        for instruction in self.instructions.iter() {
            // Note that this is not printing offsets as the book does.
            // Using the OpCode enum all the opcodes have the same size.
            // It is not space-efficient, but for now it's fine
            try!(write!(out, "{:04}", i));
            if line == self.lines[i] {
                try!(write!(out, "   |"));
            } else {
                line = self.lines[i];
                try!(write!(out, "{:4}", line));
            }
            try!(write!(out, " "));
            i = i + 1;
            try!(match instruction {
                &OpCode::Return => writeln!(out, "OP_RETURN"),
                &OpCode::Constant(offset) => {
                    writeln!(out, "OP_CONSTANT {:4} '{:}'", offset, self.values[offset])
                }
            });
        }
        Ok(())
    }
}
