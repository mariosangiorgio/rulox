use ast::{Expr, Literal};

impl Expr {
    pub fn pretty_print(&self) -> String {
        let mut pretty_printed = String::new();
        match self {
            &Expr::Literal(ref l) => pretty_printed.push_str(&l.pretty_print()),
            _ => unimplemented!(),
        }
        pretty_printed
    }
}

impl Literal {
    fn pretty_print(&self) -> String {
        match self {
            // TODO: avoid copying
            &Literal::StringLiteral(ref s) => s.clone(),
            &Literal::NumberLiteral(n) => n.to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use ast::*;

    #[test]
    fn literal() {
        let expr = Expr::Literal(Literal::StringLiteral("abc".into()));
        assert_eq!("abc", &expr.pretty_print());
    }

    #[test]
    fn complex_expression() {
        let subexpr1 = UnaryExpr {
            operator: Operator::Minus,
            right: Expr::Literal(Literal::NumberLiteral(123f64)),
        };
        let subexpr2 = Grouping { expr: Expr::Literal(Literal::NumberLiteral(45.67f64)) };
        let binary_expr = BinaryExpr {
            left: Expr::Unary(Box::new(subexpr1)),
            operator: Operator::Star,
            right: Expr::Grouping(Box::new(subexpr2)),
        };
        let expr = Expr::Binary(Box::new(binary_expr));
        assert_eq!("(* (- 123) (group 45.67))", &expr.pretty_print());
    }
}
