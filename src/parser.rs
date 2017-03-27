use ast::*;
use scanner::{Token, TokenWithContext};
use std::iter::Peekable;

pub fn parse(tokens: Vec<TokenWithContext>) -> Result<Option<Expr>, String> {
    let mut iter = tokens.iter().peekable();
    // TODO: add recovery
    parse_expression(&mut iter)
}

fn parse_expression<'a, I>(tokens: &mut Peekable<I>) -> Result<Option<Expr>, String>
    where I: Iterator<Item = &'a TokenWithContext>
{
    parse_equality(tokens)
}

fn parse_binary<'a, I>(tokens: &mut Peekable<I>,
                       map_operator: &Fn(&Token) -> Option<Operator>,
                       parse_subexpression: &Fn(&mut Peekable<I>) -> Result<Option<Expr>, String>)
                       -> Result<Option<Expr>, String>
    where I: Iterator<Item = &'a TokenWithContext>
{
    let mut expr;
    {
        if let Some(e) = try!(parse_subexpression(tokens)) {
            expr = e;
        } else {
            return Ok(None);
        }
    };
    while let Some(Some(mapped_operator)) = tokens.peek().map(|pt| map_operator(&pt.token)) {
        {
            // Just advance, we know all we need from the peeked value
            let _ = tokens.next();
        }
        let right;
        {
            if let Some(e) = try!(parse_subexpression(tokens)) {
                right = e
            } else {
                // TODO add context
                return Err("Expected subexpression".into());
            }
        };
        let binary_expression = BinaryExpr {
            left: expr,
            operator: mapped_operator,
            right: right,
        };
        expr = Expr::Binary(Box::new(binary_expression));
    }
    Ok(Some(expr))
}

fn parse_equality<'a, I>(tokens: &mut Peekable<I>) -> Result<Option<Expr>, String>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<Operator> {
        match token {
            &Token::BangEqual => Some(Operator::NotEqual),
            &Token::EqualEqual => Some(Operator::Equal),
            _ => None,
        }
    }
    parse_binary(tokens, &map_operator, &parse_comparison)
}

fn parse_comparison<'a, I>(tokens: &mut Peekable<I>) -> Result<Option<Expr>, String>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<Operator> {
        match token {
            &Token::Greater => Some(Operator::Greater),
            &Token::GreaterEqual => Some(Operator::GreaterEqual),
            &Token::Less => Some(Operator::Less),
            &Token::LessEqual => Some(Operator::LessEqual),
            _ => None,
        }
    }
    parse_binary(tokens, &map_operator, &parse_term)
}

fn parse_term<'a, I>(tokens: &mut Peekable<I>) -> Result<Option<Expr>, String>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<Operator> {
        match token {
            &Token::Minus => Some(Operator::Minus),
            &Token::Plus => Some(Operator::Plus),
            _ => None,
        }
    }
    parse_binary(tokens, &map_operator, &parse_factor)
}

fn parse_factor<'a, I>(tokens: &mut Peekable<I>) -> Result<Option<Expr>, String>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<Operator> {
        match token {
            &Token::Slash => Some(Operator::Slash),
            &Token::Star => Some(Operator::Star),
            _ => None,
        }
    }
    parse_binary(tokens, &map_operator, &parse_unary)
}

fn parse_unary<'a, I>(tokens: &mut Peekable<I>) -> Result<Option<Expr>, String>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<Operator> {
        match token {
            &Token::Minus => Some(Operator::Minus),
            &Token::Bang => Some(Operator::Bang),
            _ => None,
        }
    }
    if let Some(Some(mapped_operator)) = tokens.peek().cloned().map(|pt| map_operator(&pt.token)) {
        {
            // Just advance, we know all we need from the peeked value
            let _ = tokens.next();
        }
        let right;
        {
            if let Some(e) = try!(parse_unary(tokens)) {
                right = e;
            } else {
                // TODO: add context
                return Err("Expected right side of unary".into());
            }
        };
        let unary_expression = UnaryExpr {
            operator: mapped_operator,
            right: right,
        };
        return Ok(Some(Expr::Unary(Box::new(unary_expression))));
    } else {
        parse_primary(tokens)
    }
}

fn parse_primary<'a, I>(tokens: &mut Peekable<I>) -> Result<Option<Expr>, String>
    where I: Iterator<Item = &'a TokenWithContext>
{
    let primary_token;
    {
        primary_token = tokens.next();
    };
    if let Some(primary_token) = primary_token {
        let parsed_expression = match primary_token.token {
            Token::False => Expr::Literal(Literal::BoolLiteral(false)),
            Token::True => Expr::Literal(Literal::BoolLiteral(true)),
            Token::Nil => Expr::Literal(Literal::NilLiteral),
            Token::NumberLiteral(n) => Expr::Literal(Literal::NumberLiteral(n)),
            Token::StringLiteral(ref s) => Expr::Literal(Literal::StringLiteral(s.clone())),
            Token::LeftParen => {
                let expr;
                {
                    if let Some(e) = try!(parse_expression(tokens)) {
                        expr = e;
                    } else {
                        // TODO add context
                        return Err("Unfinished grouping expression".into());
                    }
                };
                {
                    if let Some(token) = tokens.next() {
                        if token.token == Token::LeftParen {
                            let grouping_expression = Grouping { expr: expr };
                            return Ok(Some(Expr::Grouping(Box::new(grouping_expression))));
                        }
                    }
                    // TODO: fill with context
                    return Err("Missing )".into());
                }
            }
            _ => {
                // TODO: fill with context
                return Err("Unexpected token".into());
            }
        };
        Ok(Some(parsed_expression))
    } else {
        Ok(None)
    }
}

#[cfg(test)]
mod tests {
    use scanner::*;
    use parser::*;
    use pretty_printer::PrettyPrint;

    #[test]
    fn literal() {
        let tokens = scan(&"123".into()).unwrap();
        let expr = parse(tokens).unwrap().unwrap();
        assert_eq!("123", &expr.pretty_print());
    }

    #[test]
    fn binary() {
        let tokens = scan(&"123+456".into()).unwrap();
        let expr = parse(tokens).unwrap().unwrap();
        assert_eq!("(+ 123 456)", &expr.pretty_print());
    }

    #[test]
    fn precedence_add_mul() {
        let tokens = scan(&"123+456*789".into()).unwrap();
        let expr = parse(tokens).unwrap().unwrap();
        assert_eq!("(+ 123 (* 456 789))", &expr.pretty_print());
    }

        #[test]
    fn precedence_mul_add() {
        let tokens = scan(&"123*456+789".into()).unwrap();
        let expr = parse(tokens).unwrap().unwrap();
        assert_eq!("(+ (* 123 456) 789)", &expr.pretty_print());
    }
}
