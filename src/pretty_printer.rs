use ast::{Expr, Literal, UnaryOperator, UnaryExpr, BinaryOperator, BinaryExpr, Grouping};

pub trait PrettyPrint {
    fn pretty_print_into(&self, pretty_printed: &mut String) -> ();
    fn pretty_print(&self) -> String {
        let mut pretty_printed = String::new();
        &self.pretty_print_into(&mut pretty_printed);
        pretty_printed
    }
}

impl PrettyPrint for Expr {
    fn pretty_print_into(&self, pretty_printed: &mut String) -> () {
        match self {
            &Expr::Literal(ref l) => l.pretty_print_into(pretty_printed),
            &Expr::Unary(ref u) => (*u).pretty_print_into(pretty_printed),
            &Expr::Binary(ref b) => (*b).pretty_print_into(pretty_printed),
            &Expr::Grouping(ref g) => (*g).pretty_print_into(pretty_printed),
        }
    }
}

impl PrettyPrint for UnaryOperator {
    fn pretty_print_into(&self, pretty_printed: &mut String) -> () {
        match self {
            &UnaryOperator::Bang => pretty_printed.push_str("!"),
            &UnaryOperator::Minus => pretty_printed.push_str("-"),
        }
    }
}

impl PrettyPrint for BinaryOperator {
    fn pretty_print_into(&self, pretty_printed: &mut String) -> () {
        match self {
            &BinaryOperator::Minus => pretty_printed.push_str("-"),
            &BinaryOperator::Plus => pretty_printed.push_str("+"),
            &BinaryOperator::Slash => pretty_printed.push_str("/"),
            &BinaryOperator::Star => pretty_printed.push_str("*"),
            &BinaryOperator::Equal => pretty_printed.push_str("=="),
            &BinaryOperator::NotEqual => pretty_printed.push_str("!="),
            &BinaryOperator::Less => pretty_printed.push_str("<"),
            &BinaryOperator::LessEqual => pretty_printed.push_str("<="),
            &BinaryOperator::Greater => pretty_printed.push_str(">"),
            &BinaryOperator::GreaterEqual => pretty_printed.push_str(">="),
        }
    }
}

impl PrettyPrint for Literal {
    fn pretty_print_into(&self, pretty_printed: &mut String) -> () {
        match self {
            &Literal::NilLiteral => pretty_printed.push_str("null"),
            &Literal::BoolLiteral(ref b) => pretty_printed.push_str(&b.to_string()),
            &Literal::StringLiteral(ref s) => pretty_printed.push_str(s),
            &Literal::NumberLiteral(n) => pretty_printed.push_str(&n.to_string()),
        }
    }
}

impl PrettyPrint for Grouping {
    fn pretty_print_into(&self, pretty_printed: &mut String) -> () {
        pretty_printed.push_str("(group ");
        &self.expr.pretty_print_into(pretty_printed);
        pretty_printed.push_str(")");
    }
}

impl PrettyPrint for UnaryExpr {
    fn pretty_print_into(&self, pretty_printed: &mut String) -> () {
        pretty_printed.push_str("(");
        &self.operator.pretty_print_into(pretty_printed);
        pretty_printed.push_str(" ");
        &self.right.pretty_print_into(pretty_printed);
        pretty_printed.push_str(")");
    }
}

impl PrettyPrint for BinaryExpr {
    fn pretty_print_into(&self, pretty_printed: &mut String) -> () {
        pretty_printed.push_str("(");
        &self.operator.pretty_print_into(pretty_printed);
        pretty_printed.push_str(" ");
        &self.left.pretty_print_into(pretty_printed);
        pretty_printed.push_str(" ");
        &self.right.pretty_print_into(pretty_printed);
        pretty_printed.push_str(")");
    }
}

#[cfg(test)]
mod tests {
    use ast::*;
    use pretty_printer::PrettyPrint;

    #[test]
    fn literal() {
        let expr = Expr::Literal(Literal::StringLiteral("abc".into()));
        assert_eq!("abc", &expr.pretty_print());
    }

    #[test]
    fn complex_expression() {
        let subexpr1 = UnaryExpr {
            operator: UnaryOperator::Minus,
            right: Expr::Literal(Literal::NumberLiteral(123f64)),
        };
        let subexpr2 = Grouping { expr: Expr::Literal(Literal::NumberLiteral(45.67f64)) };
        let binary_expr = BinaryExpr {
            left: Expr::Unary(Box::new(subexpr1)),
            operator: BinaryOperator::Star,
            right: Expr::Grouping(Box::new(subexpr2)),
        };
        let expr = Expr::Binary(Box::new(binary_expr));
        assert_eq!("(* (- 123) (group 45.67))", &expr.pretty_print());
    }
}
