use ast::*;
use scanner::{Lexeme, Position, Token, TokenWithContext};
use std::iter::Peekable;

/// This behave exactly as try! but wraps the returned result in a Some.
/// It's useful to remove some boilerplate in the code introduced by
/// the use of Option<Result<T, E>>
macro_rules! try_wrap_err {
    ($e:expr) => (match $e {Ok(e) => e, Err(e) => return Some(Err(e))})
}

#[derive(Debug)]
pub enum ParseError {
    UnconsumedTokens,
    UnexpectedEndOfFile,
    UnexpectedToken(Lexeme, Position),
    MissingSubexpression(Lexeme, Position),
    MissingClosingParen(Lexeme, Position),
}

pub fn parse(tokens: Vec<TokenWithContext>) -> Result<Expr, Vec<ParseError>> {
    let mut iter = tokens.iter().peekable();
    if let Some(result) = parse_expression(&mut iter) {
        match result {
            Ok(expr) => {
                if let None = iter.next() {
                    Ok(expr)
                } else {
                    Err(vec![ParseError::UnconsumedTokens])
                }
            }
            Err(error) => {
                {
                    synchronise(&mut iter);
                }
                Err(vec![error])
            }
        }

    } else {
        Err(vec![ParseError::UnexpectedEndOfFile])
    }
}

fn synchronise<'a, I>(tokens: &mut Peekable<I>)
    where I: Iterator<Item = &'a TokenWithContext>
{
    enum TokenKind {
        StartOfConstruct,
        BodyOfConstruct,
        EndOfConstruct,
    };
    fn classify(token: &Token) -> TokenKind {
        match token {
            &Token::Semicolon => TokenKind::EndOfConstruct,
            &Token::Class => TokenKind::StartOfConstruct,
            &Token::Fun => TokenKind::StartOfConstruct,
            &Token::Var => TokenKind::StartOfConstruct,
            &Token::For => TokenKind::StartOfConstruct,
            &Token::If => TokenKind::StartOfConstruct,
            &Token::While => TokenKind::StartOfConstruct,
            &Token::Print => TokenKind::StartOfConstruct,
            &Token::Return => TokenKind::StartOfConstruct,
            _ => TokenKind::BodyOfConstruct,
        }
    }
    while let Some(token_kind) = tokens.peek().map(|t| classify(&t.token)) {
        match token_kind {
            TokenKind::StartOfConstruct => {
                break;
            }
            TokenKind::BodyOfConstruct => {
                let _ = tokens.next();
            }
            TokenKind::EndOfConstruct => {
                let _ = tokens.next();
                break;
            }
        }
    }
}

fn parse_expression<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    parse_equality(tokens)
}

fn parse_binary<'a, I>(tokens: &mut Peekable<I>,
                       map_operator: &Fn(&Token) -> Option<BinaryOperator>,
                       parse_subexpression: &Fn(&mut Peekable<I>)
                                                -> Option<Result<Expr, ParseError>>)
                       -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    let mut expr;
    {
        let result = parse_subexpression(tokens);
        if let Some(Ok(subexpression)) = result {
            expr = subexpression;
        } else {
            return result;
        }
    };
    while let Some(Some(mapped_operator)) = tokens.peek().map(|pt| map_operator(&pt.token)) {
        let operator; // We know it's right, we read it just for error reporting
        {
            operator = tokens.next().unwrap();
        }
        let right;
        {
            if let Some(result) = parse_subexpression(tokens) {
                right = try_wrap_err!(result);
            } else {
                return Some(Err(ParseError::MissingSubexpression(operator.lexeme.clone(),
                                                                 operator.position)));
            }
        };
        let binary_expression = BinaryExpr {
            left: expr,
            operator: mapped_operator,
            right: right,
        };
        expr = Expr::Binary(Box::new(binary_expression));
    }
    Some(Ok(expr))
}

fn parse_equality<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<BinaryOperator> {
        match token {
            &Token::BangEqual => Some(BinaryOperator::NotEqual),
            &Token::EqualEqual => Some(BinaryOperator::Equal),
            _ => None,
        }
    }
    parse_binary(tokens, &map_operator, &parse_comparison)
}

fn parse_comparison<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<BinaryOperator> {
        match token {
            &Token::Greater => Some(BinaryOperator::Greater),
            &Token::GreaterEqual => Some(BinaryOperator::GreaterEqual),
            &Token::Less => Some(BinaryOperator::Less),
            &Token::LessEqual => Some(BinaryOperator::LessEqual),
            _ => None,
        }
    }
    parse_binary(tokens, &map_operator, &parse_term)
}

fn parse_term<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<BinaryOperator> {
        match token {
            &Token::Minus => Some(BinaryOperator::Minus),
            &Token::Plus => Some(BinaryOperator::Plus),
            _ => None,
        }
    }
    parse_binary(tokens, &map_operator, &parse_factor)
}

fn parse_factor<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<BinaryOperator> {
        match token {
            &Token::Slash => Some(BinaryOperator::Slash),
            &Token::Star => Some(BinaryOperator::Star),
            _ => None,
        }
    }
    parse_binary(tokens, &map_operator, &parse_unary)
}

fn parse_unary<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<UnaryOperator> {
        match token {
            &Token::Minus => Some(UnaryOperator::Minus),
            &Token::Bang => Some(UnaryOperator::Bang),
            _ => None,
        }
    }
    if let Some(Some(mapped_operator)) = tokens.peek().map(|pt| map_operator(&pt.token)) {
        let operator; // For error reporting
        {
            operator = tokens.next().unwrap();
        }
        let right = if let Some(result) = parse_unary(tokens) {
            try_wrap_err!(result)
        } else {
            return Some(Err(ParseError::MissingSubexpression(operator.lexeme.clone(),
                                                             operator.position)));
        };
        let unary_expression = UnaryExpr {
            operator: mapped_operator,
            right: right,
        };
        return Some(Ok(Expr::Unary(Box::new(unary_expression))));
    } else {
        parse_primary(tokens)
    }
}

fn parse_primary<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
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
                let expr = if let Some(result) = parse_expression(tokens) {
                    try_wrap_err!(result)
                } else {
                    return Some(Err(ParseError::UnexpectedEndOfFile));
                };
                {
                    if let Some(token) = tokens.next() {
                        if token.token == Token::RightParen {
                            let grouping_expression = Grouping { expr: expr };
                            return Some(Ok(Expr::Grouping(Box::new(grouping_expression))));
                        } else {
                            return Some(Err(ParseError::MissingClosingParen(token.lexeme.clone(),
                                                                            token.position)));
                        }

                    }
                    return Some(Err(ParseError::UnexpectedEndOfFile));
                }
            }
            _ => {
                return Some(Err(ParseError::UnexpectedToken(primary_token.lexeme.clone(),
                                                            primary_token.position)));
            }
        };
        Some(Ok(parsed_expression))
    } else {
        None
    }
}

#[cfg(test)]
mod tests {
    use scanner::*;
    use parser::*;
    use pretty_printer::PrettyPrint;

    #[test]
    fn literal() {
        let string = String::from("123");
        let (tokens, _) = scan(&string);
        let expr = parse(tokens).unwrap();
        assert_eq!(&string, &expr.pretty_print());
    }

    #[test]
    fn binary() {
        let (tokens, _) = scan(&"123+456");
        let expr = parse(tokens).unwrap();
        assert_eq!("(+ 123 456)", &expr.pretty_print());
    }

    #[test]
    fn precedence_add_mul() {
        let (tokens, _) = scan(&"123+456*789");
        let expr = parse(tokens).unwrap();
        assert_eq!("(+ 123 (* 456 789))", &expr.pretty_print());
    }

    #[test]
    fn precedence_mul_add() {
        let (tokens, _) = scan(&"123*456+789");
        let expr = parse(tokens).unwrap();
        assert_eq!("(+ (* 123 456) 789)", &expr.pretty_print());
    }

    #[test]
    fn precedence_mul_add_unary() {
        let (tokens, _) = scan(&"-123*456+789");
        let expr = parse(tokens).unwrap();
        assert_eq!("(+ (* (- 123) 456) 789)", &expr.pretty_print());
    }

    #[test]
    fn unclosed_group() {
        let (tokens, _) = scan(&"(2");
        assert!(parse(tokens).is_err());
    }

    #[test]
    fn unopened_group() {
        let (tokens, _) = scan(&"2)");
        assert!(parse(tokens).is_err());
    }
}
