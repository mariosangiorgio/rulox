pub enum UnaryOperator {
    Bang,
    Minus,
}

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
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
    Grouping(Box<Grouping>),
}
