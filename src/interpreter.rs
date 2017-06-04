use ast::{Expr, Literal, UnaryOperator, UnaryExpr, BinaryOperator, BinaryExpr, Grouping, Statement};
use std::collections::HashMap;
use std::io;
use std::io::prelude::*;

#[derive(Hash, Eq, PartialEq, Debug)]
struct Identifier {
    name: String, // TODO: should we use handles here?
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
}

impl Value {
    fn is_true(&self) -> bool {
        match *self {
            Value::Nil => false,
            Value::Boolean(b) => b,
            _ => true,
        }
    }

    fn to_string(&self) -> String {
        match *self {
            Value::Nil => "nil".into(),
            Value::Boolean(ref b) => b.to_string(),
            Value::Number(ref n) => n.to_string(),
            Value::String(ref s) => s.clone(),
        }
    }
}

struct Environment {
    values: HashMap<Identifier, Value>,
}

impl Environment {
    fn new() -> Environment {
        Environment { values: HashMap::new() }
    }

    fn define(&mut self, identifier: Identifier, value: Value) {
        // NOTE that this allow for variable redefinition. See the chapter.
        self.values.insert(identifier, value);
    }

    fn get(&self, identifier: &Identifier) -> Option<&Value> {
        self.values.get(identifier)
    }
}

pub struct Interpreter {
    environment: Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter { environment: Environment::new() }
    }

    pub fn execute(&mut self, statement: &Statement) -> Option<RuntimeError> {
        statement.execute(&mut self.environment)
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    UnaryMinusTypeMismatch(Value),
    BinaryOperatorTypeMismatch(BinaryOperator, Value, Value),
}

trait Interpret {
    fn interpret(&self, environment: &mut Environment) -> Result<Value, RuntimeError>;
}

trait Execute {
    fn execute(&self, environment: &mut Environment) -> Option<RuntimeError>;
}

impl Interpret for Expr {
    fn interpret(&self, environment: &mut Environment) -> Result<Value, RuntimeError> {
        match *self {
            Expr::Literal(ref l) => l.interpret(environment),
            Expr::Unary(ref u) => u.interpret(environment),
            Expr::Binary(ref b) => b.interpret(environment),
            Expr::Grouping(ref g) => g.interpret(environment),
        }
    }
}

impl Interpret for Literal {
    fn interpret(&self, _: &mut Environment) -> Result<Value, RuntimeError> {
        match *self {
            Literal::NilLiteral => Ok(Value::Nil),
            Literal::BoolLiteral(b) => Ok(Value::Boolean(b)),
            Literal::StringLiteral(ref s) => Ok(Value::String(s.clone())),
            Literal::NumberLiteral(n) => Ok(Value::Number(n)),
        }
    }
}

impl Interpret for Grouping {
    fn interpret(&self, environment: &mut Environment) -> Result<Value, RuntimeError> {
        self.expr.interpret(environment)
    }
}

impl Interpret for UnaryExpr {
    fn interpret(&self, environment: &mut Environment) -> Result<Value, RuntimeError> {
        let value = try!(self.right.interpret(environment));
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
    fn interpret(&self, environment: &mut Environment) -> Result<Value, RuntimeError> {
        let left = try!(self.left.interpret(environment));
        let right = try!(self.right.interpret(environment));
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
            (&BinaryOperator::NotEqual, l, r) => Ok(Value::Boolean(l != r)),
            (&BinaryOperator::Equal, l, r) => Ok(Value::Boolean(l == r)),
            _ => {
                Err(RuntimeError::BinaryOperatorTypeMismatch(self.operator,
                                                             left.clone(),
                                                             right.clone()))
            }
        }
    }
}

impl Execute for Statement {
    fn execute(&self, mut environment: &mut Environment) -> Option<RuntimeError> {
        match *self {
            Statement::Expression(ref e) => {
                match e.interpret(environment) {
                    Err(e) => Some(e),
                    _ => None,
                }
            }
            Statement::Print(ref e) => {
                match e.interpret(environment) {
                    Err(e) => Some(e),
                    Ok(value) => {
                        println!("{}", value.to_string());
                        let _ = io::stdout().flush(); //TODO: is this okay?
                        None
                    }
                }
            }
            Statement::VariableDefinition(ref identifier) => {
                let identifier = Identifier { name: identifier.name.clone() };
                environment.define(identifier, Value::Nil);
                None
            }
            Statement::VariableDefinitionWithInitalizer(ref identifier, ref expression) => {
                let identifier = Identifier { name: identifier.name.clone() };
                match expression.interpret(&mut environment) {
                    Ok(initializer) => {
                        environment.define(identifier, initializer);
                        None
                    }
                    Err(error) => Some(error),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::*;
    use interpreter::{Interpret, Environment, Value};

    #[test]
    fn literal() {
        let mut environment = Environment::new();
        let string = String::from("abc");
        let expr = Expr::Literal(Literal::StringLiteral(string.clone()));
        assert_eq!(Value::String(string),
                   expr.interpret(&mut environment).unwrap());
    }

    #[test]
    fn grouping() {
        let mut environment = Environment::new();
        let expr = Grouping { expr: Expr::Literal(Literal::NumberLiteral(45.67f64)) };
        assert_eq!(Value::Number(45.67f64),
                   expr.interpret(&mut environment).unwrap());
    }

    #[test]
    fn unary() {
        let mut environment = Environment::new();
        let expr = UnaryExpr {
            operator: UnaryOperator::Bang,
            right: Expr::Literal(Literal::BoolLiteral(false)),
        };
        assert_eq!(Value::Boolean(true),
                   expr.interpret(&mut environment).unwrap());
    }

    #[test]
    fn binary() {
        let mut environment = Environment::new();
        let expr = BinaryExpr {
            operator: BinaryOperator::Plus,
            left: Expr::Literal(Literal::NumberLiteral(1.0f64)),
            right: Expr::Literal(Literal::NumberLiteral(1.0f64)),
        };
        assert_eq!(Value::Number(2.0f64),
                   expr.interpret(&mut environment).unwrap());
    }

}
