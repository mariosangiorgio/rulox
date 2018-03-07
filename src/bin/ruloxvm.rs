extern crate rulox;

use rulox::vm::bytecode::{Chunk, OpCode};

fn main() {
    println!("Usage: ruloxvm [script]");
    let mut chunk = Chunk::new();
    chunk.add_instruction(OpCode::Return);

    let stdout = std::io::stdout();
    let mut handle = stdout.lock();
    let mut writer = std::io::LineWriter::new(handle);
    chunk.disassemble("test", &mut writer);
}
