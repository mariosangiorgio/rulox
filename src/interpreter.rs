use ast::*;
use lexical_scope_resolver::*;
use std::collections::HashMap;
use std::io;
use std::io::prelude::*;
use std::time::{SystemTime, UNIX_EPOCH};
use std::rc::Rc;
use std::cell::RefCell;

#[derive(Debug, PartialEq, Clone)]
pub enum Callable {
    Function(Rc<FunctionDefinition>, Environment),
    Class(Rc<Class>),
    // Native functions
    Clock,
}

#[derive(Debug, PartialEq)]
pub struct Class {
    methods: HashMap<Identifier, Callable>,
}

impl Callable {
    fn to_string(&self) -> String {
        match *self {
            Callable::Function(ref function, _) => format!("<fn {} >", function.arguments.len()),
            Callable::Class(_) => format!("<class>"),
            Callable::Clock => "clock".into(),
        }
    }

    fn register_natives(environment: &Environment, identifier_map: &mut IdentifierMap) -> () {
        environment.define(identifier_map.from_name("clock"),
                           Value::Callable(Callable::Clock))
    }

    fn bind(&self, instance: &Instance) -> Callable {
        match *self {
            Callable::Function(ref function, ref environment) => {
                let environment = Environment::new_with_parent(environment);
                environment.define(Identifier::this(), Value::Instance(instance.clone()));
                Callable::Function(function.clone(), environment)
            }
            _ => panic!(),
        }
    }

    fn call(&self,
            arguments: &Vec<Value>,
            _environment: &Environment,
            scope_resolver: &ProgramLexicalScopesResolver)
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
            Callable::Function(ref function_definition, ref environment) => {
                if arguments.len() != function_definition.arguments.len() {
                    return Err(RuntimeError::WrongNumberOfArguments);
                }
                let local_environment = Environment::new_with_parent(environment);
                for i in 0..arguments.len() {
                    local_environment.define(function_definition.arguments[i].clone(),
                                             arguments[i].clone());
                }
                let result = function_definition
                    .body
                    .execute(&local_environment, scope_resolver);
                result.map(|ok| match ok {
                               Some(value) => value,
                               None => {
                                   if function_definition.kind == FunctionKind::Initializer {
                                       environment.get(&Identifier::this(), 0).unwrap()
                                       // Special case, for when the initializer is
                                       // called explicitly to "re-initialize" the object
                                   } else {
                                       Value::Nil // For when the function didn't return
                                   }
                               }
                           })
            }
            Callable::Class(ref class) => {
                let instance = Instance::new(class);
                if let Some(initializer) = class.methods.get(&Identifier::init()) {
                    try!(initializer
                             .bind(&instance)
                             .call(arguments, _environment, scope_resolver));
                }
                Ok(Value::Instance(instance))
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct _Instance {
    class: Rc<Class>,
    fields: HashMap<Identifier, Value>,
}

// This needs to be Rc a "by reference" semantics
#[derive(Debug, PartialEq, Clone)]
pub struct Instance(Rc<RefCell<_Instance>>);

impl Instance {
    fn new(class: &Rc<Class>) -> Instance {
        Instance(Rc::new(RefCell::new(_Instance {
                                          class: class.clone(),
                                          fields: HashMap::new(),
                                      })))
    }

    fn get(&self, property: Identifier) -> Result<Value, RuntimeError> {
        if let Some(v) = self.0.borrow().fields.get(&property) {
            return Ok(v.clone());
        }
        if let Some(m) = self.find_method(property) {
            return Ok(Value::Callable(m.clone()));
        }
        Err(RuntimeError::UndefinedIdentifier(property))
    }

    fn find_method(&self, property: Identifier) -> Option<Callable> {
        let method = self.0.borrow().class.methods.get(&property).cloned();
        method.map(|m| m.bind(self))
    }

    fn set(&self, property: Identifier, value: &Value) -> () {
        self.0.borrow_mut().fields.insert(property, value.clone());
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(f64),
    String(String),
    Callable(Callable),
    Instance(Instance),
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
            //TODO: improve to_string for instance
            Value::Instance(_) => "Instance".into(),
        }
    }
}

#[derive(Debug)]
struct EnvironmentImpl {
    parent: Option<Environment>,
    values: HashMap<Identifier, Value>,
}

#[derive(Clone, Debug)]
pub struct Environment {
    actual: Rc<RefCell<EnvironmentImpl>>,
}

impl PartialEq for Environment {
    fn eq(&self, _other: &Environment) -> bool {
        false
    }
}

impl Environment {
    fn new() -> Environment {
        let actual = EnvironmentImpl {
            parent: None,
            values: HashMap::new(),
        };
        Environment { actual: Rc::new(RefCell::new(actual)) }
    }

    fn new_with_parent(parent: &Environment) -> Environment {
        let actual = EnvironmentImpl {
            parent: Some(parent.clone()),
            values: HashMap::new(),
        };
        Environment { actual: Rc::new(RefCell::new(actual)) }
    }

    fn define(&self, identifier: Identifier, value: Value) {
        // NOTE that this allow for variable redefinition. See the chapter.
        self.actual.borrow_mut().values.insert(identifier, value);
    }

    fn try_set(&self, identifier: Identifier, depth: Depth, value: Value) -> bool {
        let mut actual = self.actual.borrow_mut();
        if depth == 0 {
            if actual.values.contains_key(&identifier) {
                actual.values.insert(identifier, value);
                true
            } else {
                false
            }
        } else {
            match &actual.parent {
                &Some(ref parent) => parent.try_set(identifier, depth - 1, value),
                &None => false,
            }
        }
    }

    fn get(&self, identifier: &Identifier, depth: Depth) -> Option<Value> {
        let actual = self.actual.borrow();
        if depth == 0 {
            if let Some(value) = actual.values.get(identifier) {
                Some(value.clone())
            } else {
                None
            }
        } else {
            match &actual.parent {
                &Some(ref parent) => parent.get(identifier, depth - 1),
                &None => None,
            }
        }
    }
}

pub struct StatementInterpreter {
    environment: Environment,
}

impl StatementInterpreter {
    pub fn new(identifier_map: &mut IdentifierMap) -> StatementInterpreter {
        let environment = Environment::new();
        Callable::register_natives(&environment, identifier_map);
        StatementInterpreter { environment: environment }
    }

    pub fn execute(&mut self,
                   lexical_scope_resolver: &ProgramLexicalScopesResolver,
                   statement: &Statement)
                   -> Result<Option<Value>, RuntimeError> {
        statement.execute(&self.environment, lexical_scope_resolver)
    }
}

#[derive(Debug)]
pub enum RuntimeError {
    UnaryMinusTypeMismatch(Value),
    BinaryOperatorTypeMismatch(BinaryOperator, Value, Value),
    UndefinedIdentifier(Identifier),
    NotCallable(Value),
    NotAnInstance(Value),
    WrongNumberOfArguments,
}

trait Interpret {
    fn interpret(&self,
                 environment: &Environment,
                 &ProgramLexicalScopesResolver)
                 -> Result<Value, RuntimeError>;
}

trait Execute {
    fn execute(&self,
               environment: &Environment,
               scope_resolver: &ProgramLexicalScopesResolver)
               -> Result<Option<Value>, RuntimeError>;
}

impl Interpret for Expr {
    fn interpret(&self,
                 environment: &Environment,
                 scope_resolver: &ProgramLexicalScopesResolver)
                 -> Result<Value, RuntimeError> {
        match *self {
            Expr::Literal(ref l) => l.interpret(environment, scope_resolver),
            Expr::Unary(ref u) => u.interpret(environment, scope_resolver),
            Expr::Binary(ref b) => b.interpret(environment, scope_resolver),
            Expr::Logic(ref b) => b.interpret(environment, scope_resolver),
            Expr::Grouping(ref g) => g.interpret(environment, scope_resolver),
            Expr::This(ref handle, ref i) |
            Expr::Identifier(ref handle, ref i) => {
                match scope_resolver.get_depth(handle) {
                    Some(depth) => {
                        match environment.get(i, depth.clone()) {
                            Some(value) => Ok(value.clone()),
                            None => Err(RuntimeError::UndefinedIdentifier(i.clone())),
                        }
                    }
                    None => Err(RuntimeError::UndefinedIdentifier(i.clone())),
                }
            }
            Expr::Assignment(ref a) => a.interpret(environment, scope_resolver),
            Expr::Call(ref c) => c.interpret(environment, scope_resolver),
            Expr::Get(ref g) => {
                match g.instance.interpret(environment, scope_resolver) {
                    Ok(Value::Instance(ref instance)) => instance.get(g.property),
                    Ok(v) => Err(RuntimeError::NotAnInstance(v.clone())),
                    e => e,
                }
            }
            Expr::Set(ref s) => {
                match s.instance.interpret(environment, scope_resolver) {
                    Ok(Value::Instance(ref instance)) => {
                        let value = try!(s.value.interpret(environment, scope_resolver));
                        instance.set(s.property, &value);
                        Ok(value)
                    }
                    Ok(v) => Err(RuntimeError::NotAnInstance(v.clone())),
                    e => e,
                }
            }
        }
    }
}

impl Interpret for Literal {
    fn interpret(&self,
                 _: &Environment,
                 _: &ProgramLexicalScopesResolver)
                 -> Result<Value, RuntimeError> {
        match *self {
            Literal::NilLiteral => Ok(Value::Nil),
            Literal::BoolLiteral(b) => Ok(Value::Boolean(b)),
            Literal::StringLiteral(ref s) => Ok(Value::String(s.clone())),
            Literal::NumberLiteral(n) => Ok(Value::Number(n)),
        }
    }
}

impl Interpret for Assignment {
    fn interpret(&self,
                 environment: &Environment,
                 scope_resolver: &ProgramLexicalScopesResolver)
                 -> Result<Value, RuntimeError> {
        let target = match self.lvalue {
            Target::Identifier(ref identifier) => identifier,
        };
        match self.rvalue.interpret(environment, scope_resolver) {
            Ok(value) => {
                if let Some(depth) = scope_resolver.get_depth(&self.handle) {
                    if environment.try_set(target.clone(), depth.clone(), value.clone()) {
                        Ok(value.clone())
                    } else {
                        Err(RuntimeError::UndefinedIdentifier(*target))
                    }
                } else {
                    Err(RuntimeError::UndefinedIdentifier(*target))
                }
            }
            Err(error) => Err(error),
        }
    }
}

impl Interpret for Grouping {
    fn interpret(&self,
                 environment: &Environment,
                 scope_resolver: &ProgramLexicalScopesResolver)
                 -> Result<Value, RuntimeError> {
        self.expr.interpret(environment, scope_resolver)
    }
}

impl Interpret for UnaryExpr {
    fn interpret(&self,
                 environment: &Environment,
                 scope_resolver: &ProgramLexicalScopesResolver)
                 -> Result<Value, RuntimeError> {
        let value = try!(self.right.interpret(environment, scope_resolver));
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
    fn interpret(&self,
                 environment: &Environment,
                 scope_resolver: &ProgramLexicalScopesResolver)
                 -> Result<Value, RuntimeError> {
        let left = try!(self.left.interpret(environment, scope_resolver));
        let right = try!(self.right.interpret(environment, scope_resolver));
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
    fn interpret(&self,
                 environment: &Environment,
                 scope_resolver: &ProgramLexicalScopesResolver)
                 -> Result<Value, RuntimeError> {
        match &self.operator {
            &LogicOperator::Or => {
                let left = try!(self.left.interpret(environment, scope_resolver));
                if left.is_true() {
                    Ok(left)
                } else {
                    self.right.interpret(environment, scope_resolver)
                }
            }
            &LogicOperator::And => {
                let left = try!(self.left.interpret(environment, scope_resolver));
                if !left.is_true() {
                    Ok(left)
                } else {
                    self.right.interpret(environment, scope_resolver)
                }
            }
        }
    }
}

impl Interpret for Call {
    fn interpret(&self,
                 environment: &Environment,
                 scope_resolver: &ProgramLexicalScopesResolver)
                 -> Result<Value, RuntimeError> {
        match self.callee.interpret(environment, scope_resolver) {
            Ok(Value::Callable(c)) => {
                let mut evaluated_arguments = vec![];
                for argument in self.arguments.iter() {
                    match argument.interpret(environment, scope_resolver) {
                        Ok(value) => evaluated_arguments.push(value),
                        error => return error,
                    }
                }
                c.call(&evaluated_arguments, environment, scope_resolver)
            }
            Ok(value) => return Err(RuntimeError::NotCallable(value)),
            error => return error,
        }
    }
}

impl Execute for Statement {
    fn execute(&self,
               environment: &Environment,
               scope_resolver: &ProgramLexicalScopesResolver)
               -> Result<Option<Value>, RuntimeError> {
        match *self {
            Statement::Expression(ref e) => e.interpret(environment, scope_resolver).map(|_| None),
            // Expression statement are only for side effects
            Statement::Print(ref e) => {
                e.interpret(environment, scope_resolver)
                    .map(|value| {
                             println!("{}", value.to_string());
                             let _ = io::stdout().flush(); //TODO: is this okay?
                             None
                         })
            }
            Statement::Return(ref e) => {
                match e {
                    &Some(ref e) => e.interpret(environment, scope_resolver).map(Some),
                    &None => Ok(Some(Value::Nil)),
                }
            }
            Statement::VariableDefinition(ref identifier) => {
                environment.define(*identifier, Value::Nil);
                Ok(None)
            }
            Statement::VariableDefinitionWithInitalizer(ref identifier, ref expression) => {
                expression
                    .interpret(environment, scope_resolver)
                    .map(|initializer| {
                             environment.define(*identifier, initializer);
                             None
                         })
            }
            Statement::Block(ref b) => {
                let environment = Environment::new_with_parent(environment);
                for statement in &b.statements {
                    let result = try!(statement.execute(&environment, scope_resolver));
                    if let Some(value) = result {
                        return Ok(Some(value));
                    }
                }
                Ok(None)
            }
            Statement::IfThen(ref c) => {
                let condition = try!{c.condition.interpret(environment, scope_resolver)
                .map(|v|v.is_true())};
                if condition {
                    c.then_branch.execute(environment, scope_resolver)
                } else {
                    Ok(None)
                }
            }
            Statement::IfThenElse(ref c) => {
                let condition = try!{c.condition.interpret(environment, scope_resolver)
                .map(|v|v.is_true())};
                if condition {
                    c.then_branch.execute(environment, scope_resolver)
                } else {
                    c.else_branch.execute(environment, scope_resolver)
                }
            }
            Statement::While(ref l) => {
                while try!{l.condition.interpret(environment, scope_resolver)
                .map(|v|v.is_true())} {
                    try!{l.body.execute(environment, scope_resolver)};
                }
                Ok(None)
            }
            Statement::FunctionDefinition(ref f) => {
                environment.define(f.name.clone(),
                                   Value::Callable(Callable::Function(f.clone(),
                                                                      environment.clone())));
                Ok(None)
            }
            Statement::Class(ref c) => {
                // Not entirely sure why
                environment.define(c.name.clone(), Value::Nil);
                let mut methods = HashMap::new();
                for method_definition in c.methods.iter() {
                    methods.insert(method_definition.name,
                                   Callable::Function(method_definition.clone(),
                                                      environment.clone()));
                }
                //TODO: pass trough the class name
                let class = Rc::new(Class { methods: methods });
                environment.define(c.name.clone(), Value::Callable(Callable::Class(class)));
                Ok(None)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::*;
    use interpreter::{StatementInterpreter, Interpret, Execute, Environment, Value};
    use lexical_scope_resolver::ProgramLexicalScopesResolver;
    use scanner::*;
    use parser::*;

    //TODO: change these tests so that:
    // - they have an helper with all the repeated structure
    // - they all take a program string, parse it and interpret it

    #[test]
    fn literal() {
        let environment = Environment::new();
        let scope_resolver = ProgramLexicalScopesResolver::new();
        let string = String::from("abc");
        let expr = Expr::Literal(Literal::StringLiteral(string.clone()));
        assert_eq!(Value::String(string),
                   expr.interpret(&environment, &scope_resolver).unwrap());
    }

    #[test]
    fn grouping() {
        let environment = Environment::new();
        let scope_resolver = ProgramLexicalScopesResolver::new();
        let expr = Grouping { expr: Expr::Literal(Literal::NumberLiteral(45.67f64)) };
        assert_eq!(Value::Number(45.67f64),
                   expr.interpret(&environment, &scope_resolver).unwrap());
    }

    #[test]
    fn unary() {
        let environment = Environment::new();
        let scope_resolver = ProgramLexicalScopesResolver::new();
        let expr = UnaryExpr {
            operator: UnaryOperator::Bang,
            right: Expr::Literal(Literal::BoolLiteral(false)),
        };
        assert_eq!(Value::Boolean(true),
                   expr.interpret(&environment, &scope_resolver).unwrap());
    }

    #[test]
    fn unary_minus_only_applies_to_numbers() {
        let mut identifier_map = IdentifierMap::new();
        let mut interpreter = StatementInterpreter::new(&mut identifier_map);
        let scope_resolver = ProgramLexicalScopesResolver::new();
        let expr = UnaryExpr {
            operator: UnaryOperator::Minus,
            right: Expr::Literal(Literal::NilLiteral),
        };
        let statement = Statement::Expression(Expr::Unary(Box::new(expr)));
        assert!(interpreter.execute(&scope_resolver, &statement).is_err());
    }

    #[test]
    fn binary() {
        let environment = Environment::new();
        let scope_resolver = ProgramLexicalScopesResolver::new();
        let expr = BinaryExpr {
            operator: BinaryOperator::Plus,
            left: Expr::Literal(Literal::NumberLiteral(1.0f64)),
            right: Expr::Literal(Literal::NumberLiteral(1.0f64)),
        };
        assert_eq!(Value::Number(2.0f64),
                   expr.interpret(&environment, &scope_resolver).unwrap());
    }

    #[test]
    fn string_concatenation() {
        let environment = Environment::new();
        let scope_resolver = ProgramLexicalScopesResolver::new();
        let expr = BinaryExpr {
            operator: BinaryOperator::Plus,
            left: Expr::Literal(Literal::StringLiteral("Foo".into())),
            right: Expr::Literal(Literal::StringLiteral("Bar".into())),
        };
        assert_eq!(Value::String("FooBar".into()),
                   expr.interpret(&environment, &scope_resolver).unwrap());
    }

    #[test]
    fn binary_expression_with_mismatching_types() {
        let environment = Environment::new();
        let scope_resolver = ProgramLexicalScopesResolver::new();
        let expr = BinaryExpr {
            operator: BinaryOperator::LessEqual,
            left: Expr::Literal(Literal::NilLiteral),
            right: Expr::Literal(Literal::NumberLiteral(1.0f64)),
        };
        assert!(expr.interpret(&environment, &scope_resolver).is_err());
    }

    #[test]
    fn expression_statement() {
        let environment = Environment::new();
        let scope_resolver = ProgramLexicalScopesResolver::new();
        let expr = Expr::Literal(Literal::NumberLiteral(1.0f64));
        let statement = Statement::Expression(expr);
        assert_eq!(None,
                   statement.execute(&environment, &scope_resolver).unwrap());
    }

    #[test]
    fn complex_expression_statement() {
        let environment = Environment::new();
        let scope_resolver = ProgramLexicalScopesResolver::new();
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
        assert_eq!(None,
                   statement.execute(&environment, &scope_resolver).unwrap());
    }

    #[test]
    fn variable_definition() {
        let mut identifier_map = IdentifierMap::new();
        let environment = Environment::new();
        let scope_resolver = ProgramLexicalScopesResolver::new();
        let identifier = identifier_map.from_name(&"x");
        let statement = Statement::VariableDefinition(identifier);
        assert_eq!(None,
                   statement.execute(&environment, &scope_resolver).unwrap());
        assert_eq!(Value::Nil, environment.get(&identifier, 0).unwrap());
    }

    #[test]
    fn error_accessing_undefined_variable() {
        let mut identifier_map = IdentifierMap::new();
        let environment = Environment::new();
        let mut scope_resolver = ProgramLexicalScopesResolver::new();
        let identifier = identifier_map.from_name(&"x");
        let mut handle_factory = VariableUseHandleFactory::new();
        let statement = Statement::Expression(Expr::Identifier(handle_factory.next(), identifier));
        let _ = scope_resolver.resolve(&statement);
        assert!(statement.execute(&environment, &scope_resolver).is_err());
    }

    #[test]
    fn error_assigning_undefined_variable() {
        let mut identifier_map = IdentifierMap::new();
        let environment = Environment::new();
        let mut scope_resolver = ProgramLexicalScopesResolver::new();
        let identifier = identifier_map.from_name(&"x");
        let mut handle_factory = VariableUseHandleFactory::new();
        let statement = Statement::Expression(
            Expr::Assignment(
                Box::new(
        Assignment{
            handle: handle_factory.next(),
            lvalue: Target::Identifier(identifier),
            rvalue: Expr::Literal(Literal::BoolLiteral(true))
                        }))
                    );
        let _ = scope_resolver.resolve(&statement);
        assert!(statement.execute(&environment, &scope_resolver).is_err());
    }

    #[test]
    fn variable_definition_with_initializer() {
        let mut identifier_map = IdentifierMap::new();
        let environment = Environment::new();
        let scope_resolver = ProgramLexicalScopesResolver::new();
        let identifier = identifier_map.from_name(&"x");
        let expr = Expr::Literal(Literal::NumberLiteral(1.0f64));
        let statement = Statement::VariableDefinitionWithInitalizer(identifier.clone(), expr);
        assert_eq!(None,
                   statement.execute(&environment, &scope_resolver).unwrap());
        assert_eq!(Value::Number(1.0f64),
                   environment.get(&identifier, 0).unwrap());
    }

    #[test]
    fn block_affects_outer_scope() {
        let mut identifier_map = IdentifierMap::new();
        let environment = Environment::new();
        let mut scope_resolver = ProgramLexicalScopesResolver::new();
        let identifier = identifier_map.from_name(&"x");
        let mut handle_factory = VariableUseHandleFactory::new();
        let expr = Expr::Literal(Literal::NumberLiteral(1.0f64));
        let outer_statement = Statement::VariableDefinitionWithInitalizer(identifier.clone(), expr);
        let _ = scope_resolver.resolve(&outer_statement);
        assert_eq!(None,
                   outer_statement
                       .execute(&environment, &scope_resolver)
                       .unwrap());
        let statements = vec![
            Statement::Expression(
                Expr::Assignment(Box::new(Assignment{
                    handle: handle_factory.next(),
                    lvalue: Target::Identifier(identifier),
                    rvalue: Expr::Literal(Literal::BoolLiteral(false))})))];
        let block = Statement::Block(Box::new(Block { statements: statements }));
        let _ = scope_resolver.resolve(&block);
        assert!(block.execute(&environment, &scope_resolver).is_ok());
        assert_eq!(Value::Boolean(false),
                   environment.get(&identifier, 0).unwrap());
    }
    #[test]
    fn block_variable_dont_escape_scope() {
        let mut identifier_map = IdentifierMap::new();
        let environment = Environment::new();
        let scope_resolver = ProgramLexicalScopesResolver::new();
        let identifier = identifier_map.from_name(&"x");
        let expr = Expr::Literal(Literal::NumberLiteral(1.0f64));
        let statements = vec![Statement::VariableDefinitionWithInitalizer(identifier.clone(),
                                                                          expr)];
        let block = Statement::Block(Box::new(Block { statements: statements }));
        assert_eq!(None, block.execute(&environment, &scope_resolver).unwrap());
        // The variable declaration gets lost when we exit the scope
        assert_eq!(None, environment.get(&identifier, 0));
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
        let mut identifier_map = IdentifierMap::new();
        let environment = Environment::new();
        let scope_resolver = ProgramLexicalScopesResolver::new();
        let identifier = identifier_map.from_name(&"x");
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
        assert_eq!(None, block.execute(&environment, &scope_resolver).unwrap());
        assert_eq!(Value::Number(2.0f64),
                   environment.get(&identifier, 0).unwrap());
    }

    #[test]
    fn while_loop() {
        let mut identifier_map = IdentifierMap::new();
        let environment = Environment::new();
        let mut scope_resolver = ProgramLexicalScopesResolver::new();
        let (tokens, _) = scan(&"var a = 2; var b = 0;while(a > 0){ a = a - 1; b = b + 1;}");
        let statements = Parser::new().parse(&tokens).unwrap();
        for statement in statements.iter() {
            let _ = scope_resolver.resolve(statement);
        }
        for statement in statements.iter() {
            let _ = statement.execute(&environment, &scope_resolver);
        }
        assert_eq!(Value::Number(0.0f64),
                   environment.get(&identifier_map.from_name(&"a"), 0).unwrap());
        assert_eq!(Value::Number(2.0f64),
                   environment.get(&identifier_map.from_name(&"b"), 0).unwrap());
    }

    #[test]
    fn function_declaration_and_call() {
        let mut parser = Parser::new();
        let environment = Environment::new();
        let mut scope_resolver = ProgramLexicalScopesResolver::new();
        let (tokens, _) = scan(&"fun double(n) {return 2 * n;} var a = double(3);");
        let statements = parser.parse(&tokens).unwrap();
        for statement in statements.iter() {
            let _ = scope_resolver.resolve(statement);
        }
        for statement in statements.iter() {
            let _ = statement.execute(&environment, &scope_resolver);
        }
        assert_eq!(Value::Number(6.0f64),
                   environment
                       .get(&parser.identifier_map.from_name(&"a"), 0)
                       .unwrap());
    }

    #[test]
    fn function_declaration_and_call_no_return() {
        let mut parser = Parser::new();
        let environment = Environment::new();
        let mut scope_resolver = ProgramLexicalScopesResolver::new();
        let (tokens, _) = scan(&"fun double(n) {print 2 * n;} var a = double(3);");
        let statements = parser.parse(&tokens).unwrap();
        for statement in statements.iter() {
            let _ = scope_resolver.resolve(statement);
        }
        for statement in statements.iter() {
            let _ = statement.execute(&environment, &scope_resolver);
        }
        assert_eq!(Value::Nil,
                   environment
                       .get(&parser.identifier_map.from_name(&"a"), 0)
                       .unwrap());
    }

    #[test]
    fn local_variables_dont_pollute_outer_scope() {
        let mut parser = Parser::new();
        let environment = Environment::new();
        let mut scope_resolver = ProgramLexicalScopesResolver::new();
        let (tokens, _) = scan(&"var a = 21;fun foo(x, y) {var a = 1; var b = x + y;} foo();");
        let statements = parser.parse(&tokens).unwrap();
        for statement in statements.iter() {
            let _ = scope_resolver.resolve(statement);
        }
        for statement in statements.iter() {
            let _ = statement.execute(&environment, &scope_resolver);
        }
        assert_eq!(Value::Number(21.0f64),
                   environment
                       .get(&parser.identifier_map.from_name(&"a"), 0)
                       .unwrap());
        assert_eq!(None,
                   environment.get(&parser.identifier_map.from_name(&"b"), 0));
    }

    #[test]
    fn class_constructor() {
        let mut parser = Parser::new();
        let environment = Environment::new();
        let mut scope_resolver = ProgramLexicalScopesResolver::new();
        let (tokens, _) = scan(&"class C{} var c = C();");
        let statements = parser.parse(&tokens).unwrap();
        for statement in statements.iter() {
            let _ = scope_resolver.resolve(statement);
        }
        for statement in statements.iter() {
            let _ = statement.execute(&environment, &scope_resolver);
        }
        fn is_instance(value: &Value) -> bool {
            match *value {
                Value::Instance(_) => true,
                _ => false,
            }
        };
        assert_eq!(true,
                   is_instance(&environment
                                    .get(&parser.identifier_map.from_name(&"c"), 0)
                                    .unwrap()));
    }

    #[test]
    fn class_properties() {
        let mut parser = Parser::new();
        let environment = Environment::new();
        let mut scope_resolver = ProgramLexicalScopesResolver::new();
        let (tokens, _) = scan(&"class C{} var c = C();c.a = 10;c.b=c.a+1;");
        let statements = parser.parse(&tokens).unwrap();
        for statement in statements.iter() {
            let _ = scope_resolver.resolve(statement);
        }
        for statement in statements.iter() {
            let _ = statement.execute(&environment, &scope_resolver);
        }
        if let Value::Instance(instance) =
            environment
                .get(&parser.identifier_map.from_name(&"c"), 0)
                .unwrap() {
            assert_eq!(Value::Number(10.0),
                       instance.get(parser.identifier_map.from_name(&"a")).unwrap());
            assert_eq!(Value::Number(11.0),
                       instance.get(parser.identifier_map.from_name(&"b")).unwrap());
        } else {
            assert!(false);
        }
    }

    #[test]
    fn method_execution() {
        let mut parser = Parser::new();
        let environment = Environment::new();
        let mut scope_resolver = ProgramLexicalScopesResolver::new();
        let (tokens, _) = scan(&"class A {a() { return 1; } } var v = A().a();");
        let statements = parser.parse(&tokens).unwrap();
        for statement in statements.iter() {
            let _ = scope_resolver.resolve(statement);
        }
        for statement in statements.iter() {
            let _ = statement.execute(&environment, &scope_resolver);
        }
        assert_eq!(Value::Number(1.0),
                   environment
                       .get(&parser.identifier_map.from_name(&"v"), 0)
                       .unwrap())
    }

    #[test]
    fn custom_initializer() {
        let mut parser = Parser::new();
        let environment = Environment::new();
        let mut scope_resolver = ProgramLexicalScopesResolver::new();
        let (tokens, _) = scan(&"class A {init(a) { this._a=a; } } var v = A(10)._a;");
        let statements = parser.parse(&tokens).unwrap();
        for statement in statements.iter() {
            let _ = scope_resolver.resolve(statement);
        }
        for statement in statements.iter() {
            let _ = statement.execute(&environment, &scope_resolver);
        }
        assert_eq!(Value::Number(10.0),
                   environment
                       .get(&parser.identifier_map.from_name(&"v"), 0)
                       .unwrap())
    }
}
