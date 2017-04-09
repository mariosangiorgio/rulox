use ast::{Expr, Literal, UnaryOperator, UnaryExpr, BinaryOperator, BinaryExpr, Grouping};

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Value {
    fn is_true(&self) -> bool {
        match self {
            &Value::Nil => false,
            &Value::Boolean(b) => b,
            _ => true,
        }
    }

    fn equals(&self, other: &Value) -> bool {
        match (self, other) {
            (&Value::Nil, &Value::Nil) => true,
            (&Value::Boolean(l), &Value::Boolean(r)) => l == r,
            (&Value::Number(l), &Value::Number(r)) => l == r,
            (&Value::String(ref l), &Value::String(ref r)) => l == r,
            _ => false,
        }
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    UnaryMinusTypeMismatch(Value),
    BinaryOperatorTypeMismatch(BinaryOperator, Value, Value),
}

pub trait Interpret {
    fn interpret(&self) -> Result<Value, RuntimeError>;
}

impl Interpret for Expr {
    fn interpret(&self) -> Result<Value, RuntimeError> {
        match self {
            &Expr::Literal(ref l) => l.interpret(),
            &Expr::Unary(ref u) => u.interpret(),
            &Expr::Binary(ref b) => b.interpret(),
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

impl Interpret for UnaryExpr {
    fn interpret(&self) -> Result<Value, RuntimeError> {
        let value = try!(self.right.interpret());
        match self.operator {
            UnaryOperator::Bang => Ok(Value::Boolean(!value.is_true())),
            UnaryOperator::Minus => {
                match value {
                    Value::Number(n) => Ok(Value::Number(-n)),
                    _ => Err(RuntimeError::UnaryMinusTypeMismatch(value)),
                }
            }
        }
    }
}

impl Interpret for BinaryExpr {
    fn interpret(&self) -> Result<Value, RuntimeError> {
        let left = try!(self.left.interpret());
        let right = try!(self.right.interpret());
        match (&self.operator, &left, &right) {
            (&BinaryOperator::Minus, &Value::Number(l), &Value::Number(r)) => {
                Ok(Value::Number(l - r))
            }
            (&BinaryOperator::Slash, &Value::Number(l), &Value::Number(r)) => {
                Ok(Value::Number(l / r))
            }
            (&BinaryOperator::Star, &Value::Number(l), &Value::Number(r)) => {
                Ok(Value::Number(l * r))
            }
            (&BinaryOperator::Plus, &Value::Number(l), &Value::Number(r)) => {
                Ok(Value::Number(l + r))
            }
            (&BinaryOperator::Plus, &Value::String(ref l), &Value::String(ref r)) => {
                let mut result = String::new();
                result.push_str(l);
                result.push_str(r);
                Ok(Value::String(result))
            }
            (&BinaryOperator::Greater, &Value::Number(l), &Value::Number(r)) => {
                Ok(Value::Boolean(l > r))
            }
            (&BinaryOperator::Less, &Value::Number(l), &Value::Number(r)) => {
                Ok(Value::Boolean(l < r))
            }
            (&BinaryOperator::GreaterEqual, &Value::Number(l), &Value::Number(r)) => {
                Ok(Value::Boolean(l >= r))
            }
            (&BinaryOperator::LessEqual, &Value::Number(l), &Value::Number(r)) => {
                Ok(Value::Boolean(l <= r))
            }
            (&BinaryOperator::NotEqual, l, r) => Ok(Value::Boolean(!l.equals(r))),
            (&BinaryOperator::Equal, l, r) => Ok(Value::Boolean(l.equals(r))),
            _ => {
                Err(RuntimeError::BinaryOperatorTypeMismatch(self.operator,
                                                             left.clone(),
                                                             right.clone()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::*;
    use interpreter::{Interpret, Value};

    #[test]
    fn literal() {
        let string = String::from("abc");
        let expr = Expr::Literal(Literal::StringLiteral(string.clone()));
        assert_eq!(Value::String(string), expr.interpret().unwrap());
    }

    #[test]
    fn grouping() {
        let expr = Grouping { expr: Expr::Literal(Literal::NumberLiteral(45.67f64)) };
        assert_eq!(Value::Number(45.67f64), expr.interpret().unwrap());
    }

    #[test]
    fn unary() {
        let expr = UnaryExpr {
            operator: UnaryOperator::Bang,
            right: Expr::Literal(Literal::BoolLiteral(false)),
        };
        assert_eq!(Value::Boolean(true), expr.interpret().unwrap());
    }

    #[test]
    fn binary() {
        let expr = BinaryExpr {
            operator: BinaryOperator::Plus,
            left: Expr::Literal(Literal::NumberLiteral(1.0f64)),
            right: Expr::Literal(Literal::NumberLiteral(1.0f64)),
        };
        assert_eq!(Value::Number(2.0f64), expr.interpret().unwrap());
    }

}
