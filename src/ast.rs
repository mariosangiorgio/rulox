pub enum Operator {
    Minus,
    Star,
}

pub enum Literal {
    StringLiteral(String),
    NumberLiteral(f64),
}

pub struct UnaryExpr {
    pub operator: Operator,
    pub right: Expr,
}

pub struct BinaryExpr {
    pub left: Expr,
    pub operator: Operator,
    pub right: Expr,
}

pub struct Grouping {
    pub expr: Expr,    
}

pub enum Expr {
    Unary(Box<UnaryExpr>),
    Binary(Box<BinaryExpr>),
    Grouping(Box<Grouping>),
    Literal(Literal),
}
