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

#[derive(Hash, Eq, PartialEq, Debug, Clone)]
pub struct Identifier {
    pub name: String,
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

pub struct Grouping {
    pub expr: Expr,
}

pub enum Expr {
    Literal(Literal),
    Identifier(Identifier),
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
    Grouping(Box<Grouping>),
}

pub enum Statement {
    Print(Expr),
    Expression(Expr),
    VariableDefinition(Identifier),
    VariableDefinitionWithInitalizer(Identifier, Expr),
}
