use fnv::FnvHashMap;
use std::cmp::PartialEq;
use std::fmt::{Debug, Error, Formatter};
use std::rc::Rc;

#[derive(Debug,Clone)]
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
    pub fn super_identifier() -> Identifier {
        Identifier { handle: 2 }
    }
}

pub struct IdentifierMap {
    next: u64,
    map: FnvHashMap<String, Identifier>,
}

impl IdentifierMap {
    pub fn new() -> IdentifierMap {
        let mut map = FnvHashMap::default();
        // Reserved identifiers
        map.insert("this".into(), Identifier::this());
        map.insert("init".into(), Identifier::init());
        map.insert("super".into(), Identifier::super_identifier());
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

#[derive(Debug,Clone)]
pub enum Target {
    Identifier(Identifier),
}

#[derive(Debug, Clone)]
pub enum Literal {
    NilLiteral,
    BoolLiteral(bool),
    StringLiteral(String),
    NumberLiteral(f64),
}

#[derive(Debug,Clone)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub right: Expr,
}

#[derive(Debug,Clone)]
pub struct BinaryExpr {
    pub left: Expr,
    pub operator: BinaryOperator,
    pub right: Expr,
}

// Same as BinaryExpr, but with short-circuiting
#[derive(Debug,Clone)]
pub struct LogicExpr {
    pub left: Expr,
    pub operator: LogicOperator,
    pub right: Expr,
}

#[derive(Debug,Clone)]
pub struct Grouping {
    pub expr: Expr,
}

#[derive(Debug,Clone)]
pub struct Assignment {
    pub handle: VariableUseHandle,
    pub lvalue: Target,
    pub rvalue: Expr,
}

#[derive(Debug,Clone)]
pub struct Call {
    pub callee: Expr,
    pub arguments: Vec<Expr>,
}

#[derive(Debug,Clone)]
pub struct Get {
    pub instance: Expr,
    pub property: Identifier,
}

#[derive(Debug,Clone)]
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

#[derive(Debug, Clone)]
pub enum Expr {
    This(VariableUseHandle, Identifier),
    Super(VariableUseHandle, Identifier, Identifier),
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

#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub struct Block {
    pub statements: Vec<Statement>,
}

#[derive(Debug, Clone)]
pub struct IfThen {
    pub condition: Expr,
    pub then_branch: Statement,
}

#[derive(Debug, Clone)]
pub struct IfThenElse {
    pub condition: Expr,
    pub then_branch: Statement,
    pub else_branch: Statement,
}

#[derive(Debug, Clone)]
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

#[derive(Clone)]
pub struct ClassDefinition {
    pub name: Identifier,
    pub superclass: Option<Expr>,
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
