extern crate rulox;

use rulox::vm::bytecode::{Chunk, OpCode};

fn main() {
    println!("Usage: ruloxvm [script]");
    let mut chunk = Chunk::new();
    let offset = chunk.add_constant(1.2);
    chunk.add_instruction(OpCode::Constant(offset), 123);
    chunk.add_instruction(OpCode::Return, 123);

    let stdout = std::io::stdout();
    let handle = stdout.lock();
    let mut writer = std::io::LineWriter::new(handle);
    chunk.disassemble("test", &mut writer).unwrap();
}
