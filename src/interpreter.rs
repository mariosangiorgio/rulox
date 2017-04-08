use ast::{Expr, Literal, UnaryOperator, UnaryExpr, BinaryOperator, BinaryExpr, Grouping};

#[derive(Debug, PartialEq)]
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
            &Expr::Grouping(ref g) => g.interpret(),
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

impl Interpret for Grouping {
    fn interpret(&self) -> Result<Value, RuntimeError> {
        self.expr.interpret()
    }
}

#[cfg(test)]
mod tests {
    use ast::*;
    use interpreter::{Interpret, Value};

    #[test]
    fn literal() {
        let expr = Expr::Literal(Literal::StringLiteral("abc".into()));
        assert_eq!(Value::String("abc".into()), expr.interpret().unwrap());
    }

    #[test]
    fn grouping() {
        let expr = Grouping { expr: Expr::Literal(Literal::NumberLiteral(45.67f64)) };
        assert_eq!(Value::Number(45.67f64), expr.interpret().unwrap());
    }
}
