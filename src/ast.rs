use std::fmt::{Debug, Formatter, Error};
use std::cmp::PartialEq;
use std::rc::Rc;
use std::collections::HashMap;

#[derive(Debug)]
pub enum UnaryOperator {
    Bang,
    Minus,
}

#[derive(Debug, Clone, Copy)]
pub enum BinaryOperator {
    Minus,
    Plus,
    Slash,
    Star,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
}

#[derive(Debug, Clone, Copy)]
pub enum LogicOperator {
    Or,
    And,
}

#[derive(Hash, Eq, PartialEq, Debug, Clone, Copy)]
pub struct Identifier {
    handle: u64,
}

impl Identifier {
    pub fn this() -> Identifier {
        Identifier { handle: 0 }
    }
    pub fn init() -> Identifier {
        Identifier { handle: 1 }
    }
}

pub struct IdentifierMap {
    next: u64,
    map: HashMap<String, Identifier>,
}

impl IdentifierMap {
    pub fn new() -> IdentifierMap {
        let mut map = HashMap::new();
        // Reserved identifiers
        map.insert("this".into(), Identifier::this());
        map.insert("init".into(), Identifier::init());
        IdentifierMap {
            next: map.len() as u64,
            map: map,
        }
    }

    pub fn from_name(&mut self, name: &str) -> Identifier {
        if let Some(identifier) = self.map.get(name) {
            return *identifier;
        }
        let identifier = Identifier { handle: self.next };
        self.next = self.next + 1;
        self.map.insert(name.into(), identifier);
        identifier
    }

    pub fn lookup(&self, identifier: &Identifier) -> Option<&String> {
        // A bit slow, but it's used only for pretty printing and for errors
        self.map
            .iter()
            .find(|&(_k, v)| v == identifier)
            .map(|(k, _v)| k)
    }
}

pub enum Target {
    Identifier(Identifier),
}

pub enum Literal {
    NilLiteral,
    BoolLiteral(bool),
    StringLiteral(String),
    NumberLiteral(f64),
}

pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub right: Expr,
}

pub struct BinaryExpr {
    pub left: Expr,
    pub operator: BinaryOperator,
    pub right: Expr,
}

// Same as BinaryExpr, but with short-circuiting
pub struct LogicExpr {
    pub left: Expr,
    pub operator: LogicOperator,
    pub right: Expr,
}

pub struct Grouping {
    pub expr: Expr,
}

pub struct Assignment {
    pub handle: VariableUseHandle,
    pub lvalue: Target,
    pub rvalue: Expr,
}

pub struct Call {
    pub callee: Expr,
    pub arguments: Vec<Expr>,
}

pub struct Get {
    pub instance: Expr,
    pub property: Identifier,
}

pub struct Set {
    pub instance: Expr,
    pub property: Identifier,
    pub value: Expr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VariableUseHandle {
    value: u16,
}

/// Things got a bit messy when I added lexical scope
/// resolution so we need to track the different uses
/// of each variable and the scope they refer to.
///
/// Note that we are interested only in USES. Variable
/// declaration and definition don't have any handle!
pub struct VariableUseHandleFactory {
    next_value: u16,
}

impl VariableUseHandleFactory {
    pub fn new() -> VariableUseHandleFactory {
        VariableUseHandleFactory { next_value: 0 }
    }

    pub fn next(&mut self) -> VariableUseHandle {
        let value = self.next_value;
        self.next_value = self.next_value + 1;
        VariableUseHandle { value: value }
    }
}

pub enum Expr {
    This(VariableUseHandle, Identifier),
    Literal(Literal),
    Identifier(VariableUseHandle, Identifier),
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
    Logic(Box<LogicExpr>),
    Grouping(Box<Grouping>),
    Assignment(Box<Assignment>),
    Call(Box<Call>),
    Get(Box<Get>),
    Set(Box<Set>),
}

pub enum Statement {
    Print(Expr),
    Expression(Expr),
    Return(Option<Expr>),
    VariableDefinition(Identifier),
    VariableDefinitionWithInitalizer(Identifier, Expr),
    Block(Box<Block>),
    IfThen(Box<IfThen>),
    IfThenElse(Box<IfThenElse>),
    While(Box<While>),
    FunctionDefinition(Rc<FunctionDefinition>),
    Class(ClassDefinition),
}

pub struct Block {
    pub statements: Vec<Statement>,
}

pub struct IfThen {
    pub condition: Expr,
    pub then_branch: Statement,
}

pub struct IfThenElse {
    pub condition: Expr,
    pub then_branch: Statement,
    pub else_branch: Statement,
}

pub struct While {
    pub condition: Expr,
    pub body: Statement,
}

#[derive(Clone, Copy, PartialEq)]
pub enum FunctionKind {
    Function,
    Method,
    Initializer,
}

pub struct FunctionDefinition {
    pub kind: FunctionKind,
    pub name: Identifier,
    pub arguments: Vec<Identifier>,
    pub body: Statement, //This should be a block
}

impl Debug for FunctionDefinition {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        f.write_str("<fn>")
    }
}

impl PartialEq for FunctionDefinition {
    fn eq(&self, _other: &FunctionDefinition) -> bool {
        false
    }
}

pub struct ClassDefinition {
    pub name: Identifier,
    pub methods: Vec<Rc<FunctionDefinition>>,
}

impl Debug for ClassDefinition {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        f.write_str("<class>")
    }
}

impl PartialEq for ClassDefinition {
    fn eq(&self, _other: &ClassDefinition) -> bool {
        false
    }
}
