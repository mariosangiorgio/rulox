use ast::{Expr, Literal, Operator, UnaryExpr, BinaryExpr, Grouping};

impl Expr {
    // TODO: avoid copying of strings.
    // Possibly I can pass a mutable string to be filled
    pub fn pretty_print(&self) -> String {
        match self {
            &Expr::Literal(ref l) => l.pretty_print(),
            &Expr::Unary(ref u) => (*u).pretty_print(),
            &Expr::Binary(ref b) => (*b).pretty_print(),
            &Expr::Grouping(ref g) => (*g).pretty_print(),
        }
    }
}

impl Operator {
    fn pretty_print(&self) -> String {
        match self {
            &Operator::Minus => "-".into(),
            &Operator::Star => "*".into(),
        }
    }
}

impl Literal {
    fn pretty_print(&self) -> String {
        match self {
            &Literal::StringLiteral(ref s) => s.clone(),
            &Literal::NumberLiteral(n) => n.to_string(),
        }
    }
}

impl Grouping {
    fn pretty_print(&self) -> String {
        let mut pretty_printed = String::new();
        pretty_printed.push_str("(group ");
        pretty_printed.push_str(&self.expr.pretty_print());
        pretty_printed.push_str(")");
        pretty_printed
    }
}

impl UnaryExpr {
    fn pretty_print(&self) -> String {
        let mut pretty_printed = String::new();
        pretty_printed.push_str("(");
        pretty_printed.push_str(&self.operator.pretty_print());
        pretty_printed.push_str(" ");
        pretty_printed.push_str(&self.right.pretty_print());
        pretty_printed.push_str(")");
        pretty_printed
    }
}

impl BinaryExpr {
    fn pretty_print(&self) -> String {
        let mut pretty_printed = String::new();
        pretty_printed.push_str("(");
        pretty_printed.push_str(&self.operator.pretty_print());
        pretty_printed.push_str(" ");
        pretty_printed.push_str(&self.left.pretty_print());
        pretty_printed.push_str(" ");
        pretty_printed.push_str(&self.right.pretty_print());
        pretty_printed.push_str(")");
        pretty_printed
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
