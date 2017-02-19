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
}
