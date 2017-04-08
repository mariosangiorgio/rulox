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
            &Expr::Literal(ref l) => unimplemented!(),
            &Expr::Unary(ref u) => unimplemented!(),
            &Expr::Binary(ref b) => unimplemented!(),
            &Expr::Grouping(ref g) => unimplemented!(),
        }
    }
}
