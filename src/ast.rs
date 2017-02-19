enum Operator {

}
pub enum Literal {
    StringLiteral(String),
    NumberLiteral(f64),
}

pub enum Expr {
    Unary(Operator, Box<Expr>),
    Binary(Box<Expr>, Operator, Box<Expr>),
    Grouping(Box<Expr>),
    Literal(Literal),
}
