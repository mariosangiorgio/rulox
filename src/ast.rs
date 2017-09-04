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

pub enum Expr {
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
    VariableDefinition(Identifier),
    VariableDefinitionWithInitalizer(Identifier, Expr),
    Block(Box<Block>),
    IfThen(Box<IfThen>),
    IfThenElse(Box<IfThenElse>),
    While(Box<While>),
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
