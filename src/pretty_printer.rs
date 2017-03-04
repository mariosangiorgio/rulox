use ast::{Expr, Literal};

pub fn pretty_print(expr: &Expr) -> String {
    let mut pretty_printed = String::new();
    match expr {
        &Expr::Literal(ref l) => {
            match l {
                &Literal::StringLiteral(ref s) => pretty_printed.push_str(&s),
                &Literal::NumberLiteral(n) => pretty_printed.push_str(&n.to_string()),
            }
        }
        _ => unimplemented!(),
    }
    pretty_printed
}

#[cfg(test)]
mod tests {
    use ast::*;
    use pretty_printer::*;

    #[test]
    fn literal() {
        let expr = Expr::Literal(Literal::StringLiteral("abc".into()));
        assert_eq!("abc", pretty_print(&expr));
    }

    #[test]
    fn complex_expression() {
        let subexpr1 = UnaryExpr{
            operator: Operator::Minus,
            right: Expr::Literal(Literal::NumberLiteral(123f64))
        };
        let subexpr2 = Grouping{
            expr: Expr::Literal(Literal::NumberLiteral(45.67f64))
        };
        let binary_expr = BinaryExpr{
            left: Expr::Unary(Box::new(subexpr1)),
            operator: Operator::Star,
            right: Expr::Grouping(Box::new(subexpr2))
        };
        let expr = Expr::Binary(Box::new(binary_expr));
        assert_eq!("(* (- 123) (group 45.67))", pretty_print(&expr));
    }
}
