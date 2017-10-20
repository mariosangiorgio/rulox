use std::fmt::{Debug, Formatter, Error};
use std::cmp::PartialEq;
use std::rc::Rc;

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

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub struct Identifier {
    pub name: String,
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
    pub lvalue: Target,
    pub rvalue: Expr,
}

pub struct Call {
    pub callee: Expr,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ExpressionHandle {
    value: u16,
}

pub struct ExpressionHandleFactory {
    next_value: u16,
}

impl ExpressionHandleFactory {
    pub fn new() -> ExpressionHandleFactory {
        ExpressionHandleFactory { next_value: 0 }
    }

    pub fn next(&mut self) -> ExpressionHandle {
        let value = self.next_value;
        self.next_value = self.next_value + 1;
        ExpressionHandle { value: value }
    }
}

pub struct Expr {
    pub handle: ExpressionHandle,
    pub expr: ExprEnum,
}

pub enum ExprEnum {
    Literal(Literal),
    Identifier(Identifier),
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
    Logic(Box<LogicExpr>),
    Grouping(Box<Grouping>),
    Assignment(Box<Assignment>),
    Call(Box<Call>),
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

pub struct FunctionDefinition {
    pub name: Identifier,
    pub arguments: Vec<Identifier>,
    pub body: Statement, //This should be a block
}

impl Debug for FunctionDefinition {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        f.write_str(&self.name.name)
    }
}

impl PartialEq for FunctionDefinition {
    fn eq(&self, _other: &FunctionDefinition) -> bool {
        false
    }
}
