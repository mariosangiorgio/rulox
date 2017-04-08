use ast::{Expr, Literal, UnaryOperator, UnaryExpr, BinaryOperator, BinaryExpr, Grouping};

#[derive(Debug)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

#[derive(Debug)]
pub enum RuntimeError {
    RuntimeError,
}

pub trait Interpret {
    fn interpret(&self) -> Result<Value, RuntimeError>;
}

impl Interpret for Expr {
    fn interpret(&self) -> Result<Value, RuntimeError> {
        match self {
            &Expr::Literal(ref l) => l.interpret(),
            &Expr::Unary(ref u) => unimplemented!(),
            &Expr::Binary(ref b) => unimplemented!(),
            &Expr::Grouping(ref g) => unimplemented!(),
        }
    }
}

impl Interpret for Literal {
    fn interpret(&self) -> Result<Value, RuntimeError> {
        match self {
            &Literal::NilLiteral => Ok(Value::Nil),
            &Literal::BoolLiteral(b) => Ok(Value::Boolean(b)),
            &Literal::StringLiteral(ref s) => Ok(Value::String(s.clone())),
            &Literal::NumberLiteral(n) => Ok(Value::Number(n)),
        }
    }
}
