use ast::*;
use std::collections::HashMap;
use std::io;
use std::io::prelude::*;
use std::time::{SystemTime, UNIX_EPOCH};
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, PartialEq, Clone)]
pub enum Callable {
    Function(Rc<FunctionDefinition>),
    // Native functions
    Clock,
}

impl Callable {
    fn to_string(&self) -> String {
        match *self {
            Callable::Function(ref function) => format!("<fn {} >", &function.name.name),
            Callable::Clock => "clock".into(),
        }
    }

    fn register_natives(environment: &mut Environment) -> () {
        environment.define(Identifier { name: "clock".into() },
                           Value::Callable(Callable::Clock))
    }

    fn call(&self,
            arguments: &Vec<Value>,
            environment: &mut Environment)
            -> Result<Value, RuntimeError> {
        match *self {
            Callable::Clock => {
                if arguments.len() != 0 {
                    return Err(RuntimeError::WrongNumberOfArguments);
                }
                Ok(Value::Number(SystemTime::now()
                                     .duration_since(UNIX_EPOCH)
                                     .unwrap()
                                     .as_secs() as f64))
            }
            Callable::Function(ref function_definition) => {
                if arguments.len() != function_definition.arguments.len() {
                    return Err(RuntimeError::WrongNumberOfArguments);
                }
                let mut local_environment = environment.new_with_globals();
                for i in 0..arguments.len() {
                    local_environment.define(function_definition.arguments[i].clone(),
                                             arguments[i].clone());
                }
                let result = function_definition.body.execute(&mut local_environment);
                result.map(|ok| match ok {
                               Some(value) => value,
                               None => Value::Nil, // For when the function didn't return
                           })
            }
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Callable(Callable),
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
            Value::Callable(ref c) => c.to_string(),
        }
    }
}

struct Environment {
    // This needs to be reference counted because of:
    // - functions that need to reference the global scope
    // - closures
    values: Vec<RefCell<HashMap<Identifier, Value>>>,
}

impl Environment {
    fn new() -> Environment {
        Environment { values: vec![RefCell::new(HashMap::new())] }
    }

    fn new_with_globals(&self) -> Environment {
        Environment { values: vec![self.values[0].clone(), RefCell::new(HashMap::new())] }
    }

    fn push(&mut self) {
        self.values.push(RefCell::new(HashMap::new()));
    }

    fn pop(&mut self) {
        self.values.pop();
    }

    fn top(&self) -> usize {
        self.values.len() - 1
    }

    fn define(&mut self, identifier: Identifier, value: Value) {
        // NOTE that this allow for variable redefinition. See the chapter.
        let index = self.top();
        self.values[index].borrow_mut().insert(identifier, value);
    }

    fn try_set(&mut self, identifier: Identifier, value: Value) -> bool {
        for index in (0..self.values.len()).rev() {
            let mut values = self.values[index].borrow_mut();
            if values.contains_key(&identifier) {
                values.insert(identifier, value);
                return true;
            }
        }
        false
    }

    fn get(&self, identifier: &Identifier) -> Option<Value> {
        for index in (0..self.values.len()).rev() {
            let values = self.values[index].borrow();
            if let Some(value) = values.get(identifier) {
                return Some(value.clone());
            }
        }
        None
    }
}

pub trait Interpreter {
    fn execute(&mut self, statement: &Statement) -> Result<Option<Value>, RuntimeError>;
}

pub struct StatementInterpreter {
    environment: Environment,
}

impl StatementInterpreter {
    pub fn new() -> StatementInterpreter {
        let mut environment = Environment::new();
        Callable::register_natives(&mut environment);
        StatementInterpreter { environment: environment }
    }
}
impl Interpreter for StatementInterpreter {
    fn execute(&mut self, statement: &Statement) -> Result<Option<Value>, RuntimeError> {
        statement.execute(&mut self.environment)
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    UnaryMinusTypeMismatch(Value),
    BinaryOperatorTypeMismatch(BinaryOperator, Value, Value),
    UndefinedIdentifier(Identifier),
    NotCallable(Value),
    WrongNumberOfArguments,
}

trait Interpret {
    fn interpret(&self, environment: &mut Environment) -> Result<Value, RuntimeError>;
}

trait Execute {
    fn execute(&self, environment: &mut Environment) -> Result<Option<Value>, RuntimeError>;
}

impl Interpret for Expr {
    fn interpret(&self, environment: &mut Environment) -> Result<Value, RuntimeError> {
        match *self {
            Expr::Literal(ref l) => l.interpret(environment),
            Expr::Unary(ref u) => u.interpret(environment),
            Expr::Binary(ref b) => b.interpret(environment),
            Expr::Logic(ref b) => b.interpret(environment),
            Expr::Grouping(ref g) => g.interpret(environment),
            Expr::Identifier(ref i) => i.interpret(environment),
            Expr::Assignment(ref a) => a.interpret(environment),
            Expr::Call(ref c) => c.interpret(environment),
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

impl Interpret for Identifier {
    fn interpret(&self, environment: &mut Environment) -> Result<Value, RuntimeError> {
        match environment.get(self) {
            Some(value) => Ok(value.clone()),
            None => Err(RuntimeError::UndefinedIdentifier(self.clone())),
        }
    }
}

impl Interpret for Assignment {
    fn interpret(&self, environment: &mut Environment) -> Result<Value, RuntimeError> {
        let target = match self.lvalue {
            Target::Identifier(ref i) => Identifier { name: i.name.clone() },
        };
        match self.rvalue.interpret(environment) {
            Ok(value) => {
                if environment.try_set(target.clone(), value.clone()) {
                    Ok(value.clone())
                } else {
                    Err(RuntimeError::UndefinedIdentifier(target))
                }
            }
            Err(error) => Err(error),
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

impl Interpret for LogicExpr {
    fn interpret(&self, environment: &mut Environment) -> Result<Value, RuntimeError> {
        match &self.operator {
            &LogicOperator::Or => {
                let left = try!(self.left.interpret(environment));
                if left.is_true() {
                    Ok(left)
                } else {
                    self.right.interpret(environment)
                }
            }
            &LogicOperator::And => {
                let left = try!(self.left.interpret(environment));
                if !left.is_true() {
                    Ok(left)
                } else {
                    self.right.interpret(environment)
                }
            }
        }
    }
}

impl Interpret for Call {
    fn interpret(&self, environment: &mut Environment) -> Result<Value, RuntimeError> {
        match self.callee.interpret(environment) {
            Ok(Value::Callable(c)) => {
                let mut evaluated_arguments = vec![];
                for argument in self.arguments.iter() {
                    match argument.interpret(environment) {
                        Ok(value) => evaluated_arguments.push(value),
                        error => return error,
                    }
                }
                c.call(&evaluated_arguments, environment)
            }
            Ok(value) => return Err(RuntimeError::NotCallable(value)),
            error => return error,
        }
    }
}

impl Execute for Statement {
    fn execute(&self, mut environment: &mut Environment) -> Result<Option<Value>, RuntimeError> {
        match *self {
            Statement::Expression(ref e) => e.interpret(environment).map(|_| None),
            // Expression statement are only for side effects
            Statement::Print(ref e) => {
                e.interpret(environment)
                    .map(|value| {
                             println!("{}", value.to_string());
                             let _ = io::stdout().flush(); //TODO: is this okay?
                             None
                         })
            }
            Statement::Return(ref e) => {
                match e {
                    &Some(ref e) => e.interpret(environment).map(Some),
                    &None => Ok(Some(Value::Nil)),
                }
            }
            Statement::VariableDefinition(ref identifier) => {
                let identifier = Identifier { name: identifier.name.clone() };
                environment.define(identifier, Value::Nil);
                Ok(None)
            }
            Statement::VariableDefinitionWithInitalizer(ref identifier, ref expression) => {
                let identifier = Identifier { name: identifier.name.clone() };
                expression
                    .interpret(environment)
                    .map(|initializer| {
                             environment.define(identifier, initializer);
                             None
                         })
            }
            Statement::Block(ref b) => {
                environment.push();
                for statement in &b.statements {
                    let result = try!(statement.execute(environment));
                    if let Some(value) = result {
                        environment.pop();
                        return Ok(Some(value));
                    }
                }
                environment.pop();
                Ok(None)
            }
            Statement::IfThen(ref c) => {
                let condition = try!{c.condition.interpret(environment).map(|v|v.is_true())};
                if condition {
                    c.then_branch.execute(environment)
                } else {
                    Ok(None)
                }
            }
            Statement::IfThenElse(ref c) => {
                let condition = try!{c.condition.interpret(environment).map(|v|v.is_true())};
                if condition {
                    c.then_branch.execute(environment)
                } else {
                    c.else_branch.execute(environment)
                }
            }
            Statement::While(ref l) => {
                while try!{l.condition.interpret(environment).map(|v|v.is_true())} {
                    try!{l.body.execute(environment)};
                }
                Ok(None)
            }
            Statement::FunctionDefinition(ref f) => {
                environment.define(f.name.clone(),
                                   Value::Callable(Callable::Function(f.clone())));
                Ok(None)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::*;
    use interpreter::{Interpreter, StatementInterpreter, Interpret, Execute, Environment, Value};
    use scanner::*;
    use parser::*;

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
    fn unary_minus_only_applies_to_numbers() {
        let mut interpreter = StatementInterpreter::new();
        let expr = UnaryExpr {
            operator: UnaryOperator::Minus,
            right: Expr::Literal(Literal::NilLiteral),
        };
        let statement = Statement::Expression(Expr::Unary(Box::new(expr)));
        assert!(interpreter.execute(&statement).is_err());
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

    #[test]
    fn string_concatenation() {
        let mut environment = Environment::new();
        let expr = BinaryExpr {
            operator: BinaryOperator::Plus,
            left: Expr::Literal(Literal::StringLiteral("Foo".into())),
            right: Expr::Literal(Literal::StringLiteral("Bar".into())),
        };
        assert_eq!(Value::String("FooBar".into()),
                   expr.interpret(&mut environment).unwrap());
    }

    #[test]
    fn binary_expression_with_mismatching_types() {
        let mut environment = Environment::new();
        let expr = BinaryExpr {
            operator: BinaryOperator::LessEqual,
            left: Expr::Literal(Literal::NilLiteral),
            right: Expr::Literal(Literal::NumberLiteral(1.0f64)),
        };
        assert!(expr.interpret(&mut environment).is_err());
    }

    #[test]
    fn expression_statement() {
        let mut environment = Environment::new();
        let expr = Expr::Literal(Literal::NumberLiteral(1.0f64));
        let statement = Statement::Expression(expr);
        assert_eq!(None, statement.execute(&mut environment).unwrap());
    }

    #[test]
    fn complex_expression_statement() {
        let mut environment = Environment::new();
        let subexpr1 = UnaryExpr {
            operator: UnaryOperator::Minus,
            right: Expr::Literal(Literal::NumberLiteral(2f64)),
        };
        let subexpr2 = Grouping { expr: Expr::Literal(Literal::NumberLiteral(5f64)) };
        let binary_expr = BinaryExpr {
            left: Expr::Unary(Box::new(subexpr1)),
            operator: BinaryOperator::Star,
            right: Expr::Grouping(Box::new(subexpr2)),
        };
        let expr = Expr::Binary(Box::new(binary_expr));
        let statement = Statement::Expression(expr);
        assert_eq!(None, statement.execute(&mut environment).unwrap());
    }

    #[test]
    fn variable_definition() {
        let mut environment = Environment::new();
        let identifier = Identifier { name: "x".into() };
        let statement = Statement::VariableDefinition(identifier.clone());
        assert_eq!(None, statement.execute(&mut environment).unwrap());
        assert_eq!(Value::Nil, environment.get(&identifier).unwrap());
    }

    #[test]
    fn error_accessing_undefined_variable() {
        let mut environment = Environment::new();
        let identifier = Identifier { name: "x".into() };
        let statement = Statement::Expression(Expr::Identifier(identifier));
        assert!(statement.execute(&mut environment).is_err());
    }

    #[test]
    fn error_assigning_undefined_variable() {
        let mut environment = Environment::new();
        let identifier = Identifier { name: "x".into() };
        let statement = Statement::Expression(
            Expr::Assignment(Box::new(Assignment{
                lvalue: Target::Identifier(identifier),
                rvalue: Expr::Literal(Literal::BoolLiteral(true))}))
            );
        assert!(statement.execute(&mut environment).is_err());
    }

    #[test]
    fn variable_definition_with_initializer() {
        let mut environment = Environment::new();
        let identifier = Identifier { name: "x".into() };
        let expr = Expr::Literal(Literal::NumberLiteral(1.0f64));
        let statement = Statement::VariableDefinitionWithInitalizer(identifier.clone(), expr);
        assert_eq!(None, statement.execute(&mut environment).unwrap());
        assert_eq!(Value::Number(1.0f64), environment.get(&identifier).unwrap());
    }

    #[test]
    fn block_affects_outer_scope() {
        let mut environment = Environment::new();
        let identifier = Identifier { name: "x".into() };
        let expr = Expr::Literal(Literal::NumberLiteral(1.0f64));
        let outer_statement = Statement::VariableDefinitionWithInitalizer(identifier.clone(), expr);
        assert_eq!(None, outer_statement.execute(&mut environment).unwrap());

        let statements = vec![
            Statement::Expression(
                Expr::Assignment(Box::new(Assignment{
                    lvalue: Target::Identifier(Identifier{name: "x".into()}),
                    rvalue: Expr::Literal(Literal::BoolLiteral(false))})))];
        let block = Statement::Block(Box::new(Block { statements: statements }));
        assert!(block.execute(&mut environment).is_ok());
        assert_eq!(Value::Boolean(false), environment.get(&identifier).unwrap());
    }
    #[test]
    fn block_variable_dont_escape_scope() {
        let mut environment = Environment::new();
        let identifier = Identifier { name: "x".into() };
        let expr = Expr::Literal(Literal::NumberLiteral(1.0f64));
        let statements = vec![Statement::VariableDefinitionWithInitalizer(identifier.clone(),
                                                                          expr)];
        let block = Statement::Block(Box::new(Block { statements: statements }));
        assert_eq!(None, block.execute(&mut environment).unwrap());
        // The variable declaration gets lost when we exit the scope
        assert_eq!(None, environment.get(&identifier));
    }

    #[test]
    fn truth_table() {
        // Note that lox follows Ruby's rules, not Javascript's
        assert_eq!(false, Value::Nil.is_true());
        assert_eq!(true, Value::Boolean(true).is_true());
        assert_eq!(false, Value::Boolean(false).is_true());
        assert_eq!(true, Value::Number(1.0f64).is_true());
        assert_eq!(true, Value::Number(0.0f64).is_true());
        assert_eq!(true, Value::String("".into()).is_true());
        assert_eq!(true, Value::String("Hello World!".into()).is_true());
    }

    #[test]
    fn to_string() {
        // Note that lox follows Ruby's rules, not Javascript's
        assert_eq!("nil", Value::Nil.to_string());
        assert_eq!("true", Value::Boolean(true).to_string());
        assert_eq!("false", Value::Boolean(false).to_string());
        assert_eq!("1", Value::Number(1.0f64).to_string());
        assert_eq!("0", Value::Number(0.0f64).to_string());
        assert_eq!("", Value::String("".into()).to_string());
        assert_eq!("Hello World!",
                   Value::String("Hello World!".into()).to_string());
    }

    #[test]
    fn if_then_else() {
        let mut environment = Environment::new();
        let identifier = Identifier { name: "x".into() };
        let condition = Expr::Literal(Literal::BoolLiteral(false));
        let then_expr = Expr::Literal(Literal::NumberLiteral(1.0f64));
        let else_expr = Expr::Literal(Literal::NumberLiteral(2.0f64));
        let then_statement = Statement::VariableDefinitionWithInitalizer(identifier.clone(),
                                                                         then_expr);
        let else_statement = Statement::VariableDefinitionWithInitalizer(identifier.clone(),
                                                                         else_expr);
        let block = Statement::IfThenElse(Box::new(IfThenElse {
                                                       condition: condition,
                                                       then_branch: then_statement,
                                                       else_branch: else_statement,
                                                   }));
        assert_eq!(None, block.execute(&mut environment).unwrap());
        assert_eq!(Value::Number(2.0f64), environment.get(&identifier).unwrap());
    }

    #[test]
    fn while_loop() {
        let mut environment = Environment::new();
        let (tokens, _) = scan(&"var a = 2; var b = 0;while(a > 0){ a = a - 1; b = b + 1;}");
        let statements = parse(&tokens).unwrap();
        for statement in statements {
            let _ = statement.execute(&mut environment);
        }
        assert_eq!(Value::Number(0.0f64),
                   environment.get(&Identifier { name: "a".into() }).unwrap());
        assert_eq!(Value::Number(2.0f64),
                   environment.get(&Identifier { name: "b".into() }).unwrap());
    }

    #[test]
    fn function_declaration_and_call() {
        let mut environment = Environment::new();
        let (tokens, _) = scan(&"fun double(n) {return 2 * n;} var a = double(3);");
        let statements = parse(&tokens).unwrap();
        for statement in statements {
            let _ = statement.execute(&mut environment);
        }
        assert_eq!(Value::Number(6.0f64),
                   environment.get(&Identifier { name: "a".into() }).unwrap());
    }

    #[test]
    fn function_declaration_and_call_no_return() {
        let mut environment = Environment::new();
        let (tokens, _) = scan(&"fun double(n) {print 2 * n;} var a = double(3);");
        let statements = parse(&tokens).unwrap();
        for statement in statements {
            let _ = statement.execute(&mut environment);
        }
        assert_eq!(Value::Nil,
                   environment.get(&Identifier { name: "a".into() }).unwrap());
    }
}
