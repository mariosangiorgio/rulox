use std::f64::EPSILON;
use std::io::{Error, LineWriter, Write};
use std::rc::Rc;
use vm::bytecode::{disassemble_instruction, BinaryOp, Chunk, Constant, OpCode};

/// A Lox value, which could be either a value
/// or a reference type.
#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Nil,
    Object(ObjectReference),
}
/// Reference types.
/// TODO: this should probably be a refcell, we will want to mutate objects
/// TODO: this should be a reference to a (GcMetadata, ObjectValue)
pub type ObjectReference = Rc<ObjectValue>;
#[derive(Debug, Clone, PartialEq)]
pub enum ObjectValue {
    String(String),
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
    TypeError,
}

struct Vm<'a> {
    chunk: &'a Chunk,
    program_counter: usize,
    stack: Vec<Value>,
    /// Allocated objects so the GC can keep track of them.
    /// The variants of ObjectReference is a ref-counted
    /// pointer to their actual data. That is not enough to
    /// guarantee that we free all the memory because the object
    /// graph might contain cycles.
    /// TODO: not sure if this should be weak or not
    objects: Vec<ObjectReference>,
}

impl<'a> Vm<'a> {
    fn new(chunk: &'a Chunk) -> Vm<'a> {
        Vm {
            chunk,
            program_counter: 0,
            stack: vec![],
            objects: vec![],
        }
    }

    fn pop(&mut self) -> Result<Value, RuntimeError> {
        if let Some(value) = self.stack.pop() {
            Ok(value)
        } else {
            Err(RuntimeError::StackUnderflow)
        }
    }

    /// Takes ownership of the string and creates the corresponding Lox
    /// reference type.
    /// This method also takes care of all the book-keeping required by
    /// the garbage collector.
    fn allocate_string(&mut self, value: String) -> ObjectReference {
        let o = Rc::new(ObjectValue::String(value));
        self.objects.push(o.clone());
        o
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
        if self.program_counter > self.chunk.instruction_count() {
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
                let value = match self.chunk.get_value(offset) {
                    Constant::Number(n) => Value::Number(*n),
                    Constant::Bool(b) => Value::Bool(*b),
                    Constant::Nil => Value::Nil,
                    Constant::String(ref s) => Value::Object(self.allocate_string(s.clone())),
                };
                self.stack.push(value)
            }
            OpCode::Negate => {
                let op = try!(self.pop());
                match op {
                    Value::Number(n) => self.stack.push(Value::Number(-n)),
                    _ => return Err(RuntimeError::TypeError),
                };
            }
            OpCode::Not => {
                let op = try!(self.pop());
                match op {
                    Value::Bool(b) => self.stack.push(Value::Bool(!b)),
                    Value::Nil => self.stack.push(Value::Bool(true)),
                    _ => return Err(RuntimeError::TypeError),
                };
            }
            OpCode::Binary(ref operator) => {
                // Note the order!
                // Op2 is the topmost element of the stack,
                // Op1 is the second topmost element
                let op2 = try!(self.pop());
                let op1 = try!(self.pop());
                let result = match (op1, op2) {
                    (Value::Number(op1), Value::Number(op2)) => match *operator {
                        BinaryOp::Add => Value::Number(op1 + op2),
                        BinaryOp::Subtract => Value::Number(op1 - op2),
                        BinaryOp::Multiply => Value::Number(op1 * op2),
                        BinaryOp::Divide => Value::Number(op1 / op2),
                        BinaryOp::Equals => Value::Bool((op1 - op2).abs() < EPSILON),
                        BinaryOp::NotEqual => Value::Bool((op1 - op2).abs() >= EPSILON),
                        BinaryOp::Greater => Value::Bool(op1 > op2),
                        BinaryOp::GreaterEqual => Value::Bool(op1 >= op2),
                        BinaryOp::Less => Value::Bool(op1 < op2),
                        BinaryOp::LessEqual => Value::Bool(op1 <= op2),
                        _ => return Err(RuntimeError::TypeError),
                    },
                    (Value::Bool(op1), Value::Bool(op2)) => match *operator {
                        BinaryOp::Equals => Value::Bool(op1 == op2),
                        BinaryOp::NotEqual => Value::Bool(op1 != op2),
                        BinaryOp::Or => Value::Bool(op1 || op2),
                        BinaryOp::And => Value::Bool(op1 && op2),
                        _ => return Err(RuntimeError::TypeError),
                    },
                    (Value::Nil, Value::Nil) => match *operator {
                        BinaryOp::Equals => Value::Bool(true),
                        BinaryOp::NotEqual => Value::Bool(false),
                        _ => return Err(RuntimeError::TypeError),
                    },
                    (Value::Object(v1), Value::Object(v2)) => match (&*v1, &*v2) {
                        (ObjectValue::String(ref s1), ObjectValue::String(ref s2)) => {
                            match *operator {
                                BinaryOp::Equals => Value::Bool(s1 == s2),
                                BinaryOp::NotEqual => Value::Bool(s1 != s2),
                                BinaryOp::Add => {
                                    let mut result = s1.clone();
                                    result.push_str(&*s2);
                                    Value::Object(self.allocate_string(result))
                                }
                                _ => return Err(RuntimeError::TypeError),
                            }
                        }
                    },
                    _ => return Err(RuntimeError::TypeError),
                };
                self.stack.push(result);
            }
        };
        Ok(true)
    }
    fn trace<T>(&mut self, out: &mut LineWriter<T>) -> Result<(), Error>
    where
        T: Write,
    {
        try!(write!(out, "Program Counter: {}", self.program_counter));
        try!(writeln!(out));
        try!(write!(out, "Stack: "));
        for value in &self.stack {
            try!(write!(out, "[ {:?} ]", value));
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
    use vm::interpreter::{interpret, trace};

    fn arb_constants(max_constants: usize) -> VecStrategy<BoxedStrategy<Constant>> {
        prop::collection::vec(
            prop_oneof![
                prop::num::f64::ANY.prop_map(Constant::Number),
                prop::bool::ANY.prop_map(Constant::Bool),
            ].boxed(),
            0..max_constants,
        )
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
            let mut chunk = Chunk::default();
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
        let _ = interpret(chunk);
    }
    }

    proptest! {
    #[test]
    fn trace_doesnt_crash(ref chunk in arb_chunk(10, 20)) {
        let mut writer = LineWriter::new(sink());
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

// The following are end to end tests that cover everything.
// They start from a string, compile them (thus covering the code)
// in compiler.rs, and finally run the generated chunk.
//
// This is the easiest way to check that everything works properly.
// Testing bytecode generation and interpretation separately would
// require lots of boilerplate for little benefit.
#[cfg(test)]
mod end_to_end_tests {
    use vm::compiler::compile;
    use vm::interpreter::{Value, Vm};

    #[test]
    pub fn number() {
        let chunk = compile("5").unwrap();
        let mut vm = Vm::new(&chunk);

        let _ = vm.interpret_next().unwrap();

        assert_eq!(Value::Number(5.0), vm.pop().unwrap());
    }

    #[test]
    pub fn unary() {
        let chunk = compile("-5").unwrap();
        let mut vm = Vm::new(&chunk);

        let _ = vm.interpret_next().unwrap(); // Puts 5 on the stack
        let _ = vm.interpret_next().unwrap(); // Negates it

        assert_eq!(Value::Number(-5.0), vm.pop().unwrap());
    }

    #[test]
    pub fn unary_bool() {
        let chunk = compile("!true").unwrap();
        let mut vm = Vm::new(&chunk);

        let _ = vm.interpret_next().unwrap(); // Puts 5 on the stack
        let _ = vm.interpret_next().unwrap(); // Negates it

        assert_eq!(Value::Bool(false), vm.pop().unwrap());
    }

    #[test]
    pub fn binary() {
        let chunk = compile("5+10").unwrap();
        let mut vm = Vm::new(&chunk);

        let _ = vm.interpret_next().unwrap(); // Puts 5 on the stack
        let _ = vm.interpret_next().unwrap(); // Puts 10 on the stack
        let _ = vm.interpret_next().unwrap(); // Adds them

        assert_eq!(Value::Number(15.0), vm.pop().unwrap());
    }

    #[test]
    pub fn binary_bool() {
        let chunk = compile("true or false").unwrap();
        let mut vm = Vm::new(&chunk);

        let _ = vm.interpret_next().unwrap(); // Puts true on the stack
        let _ = vm.interpret_next().unwrap(); // Puts false on the stack
        let _ = vm.interpret_next().unwrap(); // Or

        assert_eq!(Value::Bool(true), vm.pop().unwrap());
    }

    #[test]
    pub fn grouping() {
        let chunk = compile("(5+10)*3").unwrap();
        let mut vm = Vm::new(&chunk);

        let _ = vm.interpret_next().unwrap(); // Puts 5 on the stack
        let _ = vm.interpret_next().unwrap(); // Puts 10 on the stack
        let _ = vm.interpret_next().unwrap(); // Addition
        let _ = vm.interpret_next().unwrap(); // Puts 3 on the stack
        let _ = vm.interpret_next().unwrap(); // Multiplication

        assert_eq!(Value::Number(45.0), vm.pop().unwrap());
    }

    #[test]
    pub fn precedence() {
        let chunk = compile("-5+10*3").unwrap();
        let mut vm = Vm::new(&chunk);

        let _ = vm.interpret_next().unwrap(); // Puts 5 on the stack
        let _ = vm.interpret_next().unwrap(); // Negates it
        let _ = vm.interpret_next().unwrap(); // Puts 10 on the stack
        let _ = vm.interpret_next().unwrap(); // Puts 3 on the stack
        let _ = vm.interpret_next().unwrap(); // Multiplication
        let _ = vm.interpret_next().unwrap(); // Addition

        assert_eq!(Value::Number(25.0), vm.pop().unwrap());
    }

    #[test]
    pub fn complex() {
        let chunk = compile("!(5 - 4 > 3 * 2 == !nil)").unwrap();
        let mut vm = Vm::new(&chunk);

        let _ = vm.interpret_next().unwrap(); // Puts 5 on the stack
        let _ = vm.interpret_next().unwrap(); // Puts 4 on the stack
        let _ = vm.interpret_next().unwrap(); // Subtract
        let _ = vm.interpret_next().unwrap(); // Puts 2 on the stack
        let _ = vm.interpret_next().unwrap(); // Puts 3 on the stack
        let _ = vm.interpret_next().unwrap(); // Multiply
        let _ = vm.interpret_next().unwrap(); // Greater
        let _ = vm.interpret_next().unwrap(); // Puts Nil on the stack
        let _ = vm.interpret_next().unwrap(); // Not
        let _ = vm.interpret_next().unwrap(); // Equals
        let _ = vm.interpret_next().unwrap(); // Not

        assert_eq!(Value::Bool(true), vm.pop().unwrap());
    }
}
