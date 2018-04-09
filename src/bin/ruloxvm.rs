extern crate rulox;

use rulox::vm::bytecode::{disassemble, Chunk, OpCode};
use rulox::vm::vm::{trace};

fn main() {
    println!("Usage: ruloxvm [script]");
    let mut chunk = Chunk::new();
    let offset = chunk.add_constant(1.2);
    chunk.add_instruction(OpCode::Constant(offset), 123);
    chunk.add_instruction(OpCode::Negate, 123);
    chunk.add_instruction(OpCode::Return, 123);

    let stdout = std::io::stdout();
    let handle = stdout.lock();
    let mut writer = std::io::LineWriter::new(handle);
    disassemble(&chunk, "test", &mut writer).unwrap();
    trace(&chunk).unwrap();
}
