extern crate rulox;

use rulox::vm::bytecode::{disassemble, BinaryOp, Chunk, OpCode};
use rulox::vm::vm::trace;

fn main() {
    println!("Usage: ruloxvm [script]");
    let mut chunk = Chunk::new();
    let mut offset = chunk.add_constant(1.2);
    chunk.add_instruction(OpCode::Constant(offset), 123);
    offset = chunk.add_constant(3.4);
    chunk.add_instruction(OpCode::Constant(offset), 123);
    chunk.add_instruction(OpCode::Binary(BinaryOp::Add), 123);
    offset = chunk.add_constant(5.6);
    chunk.add_instruction(OpCode::Constant(offset), 123);
    chunk.add_instruction(OpCode::Binary(BinaryOp::Divide), 123);
    chunk.add_instruction(OpCode::Negate, 123);
    chunk.add_instruction(OpCode::Return, 123);

    let stdout = std::io::stdout();
    let handle = stdout.lock();
    let mut writer = std::io::LineWriter::new(handle);
    disassemble(&chunk, "test", &mut writer).unwrap();
    trace(&chunk).unwrap();
}
