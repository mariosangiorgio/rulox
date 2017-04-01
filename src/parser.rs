use ast::*;
use scanner::{Token, TokenWithContext};
use std::iter::Peekable;

/// This behave exactly as try! but wraps the returned result in a Some.
/// It's useful to remove some boilerplate in the code introduced by
/// the use of Option<Result<T, E>>
macro_rules! try_wrap_err {
    ($e:expr) => (match $e {Ok(e) => e, Err(e) => return Some(Err(e))})
}

#[derive(Debug)]
pub struct ParseError {
    message: String,
}

impl ParseError {
    fn from_message(message: String) -> ParseError {
        ParseError { message: message }
    }
}

pub fn parse(tokens: Vec<TokenWithContext>) -> Result<Expr, Vec<ParseError>> {
    let mut iter = tokens.iter().peekable();
    if let Some(result) = parse_expression(&mut iter) {
        match result {
            Ok(expr) => {
                if let None = iter.next() {
                    Ok(expr)
                } else {
                    Err(vec![ParseError::from_message("Parser didn't consume all the tokens"
                                 .into())])
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
        Err(vec![ParseError::from_message("Parser didn't return anything".into())])
    }
}

fn synchronise<'a, I>(tokens: &mut Peekable<I>)
    where I: Iterator<Item = &'a TokenWithContext>
{
    while let Some(token_with_context) = tokens.peek().cloned() {
        match token_with_context.token {
            Token::Semicolon => {
                let _ = tokens.next();
                break;
            }
            Token::Class => break,
            Token::Fun => break,
            Token::Var => break,
            Token::For => break,
            Token::If => break,
            Token::While => break,
            Token::Print => break,
            Token::Return => break,
            _ => {
                let _ = tokens.next();
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
                       map_operator: &Fn(&Token) -> Option<Operator>,
                       parse_subexpression: &Fn(&mut Peekable<I>)
                                                -> Option<Result<Expr, ParseError>>)
                       -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    let mut expr;
    {
        let result = parse_subexpression(tokens);
        if let Some(Ok(subexpression)) =  result{
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
                return Some(Err(ParseError::from_message(format!("Expected subexpression \
                                                                  after {:?} at {:?}",
                                                                 operator.lexeme,
                                                                 operator.position))));
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
    fn map_operator(token: &Token) -> Option<Operator> {
        match token {
            &Token::BangEqual => Some(Operator::NotEqual),
            &Token::EqualEqual => Some(Operator::Equal),
            _ => None,
        }
    }
    parse_binary(tokens, &map_operator, &parse_comparison)
}

fn parse_comparison<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
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

fn parse_term<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
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

fn parse_factor<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
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

fn parse_unary<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
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
        let operator; // For error reporting
        {
            operator = tokens.next().unwrap();
        }
        let right;
        {
            if let Some(result) = parse_unary(tokens) {
                right = try_wrap_err!(result);
            } else {
                return Some(Err(ParseError::from_message(format!("Expected subexpression \
                                                                  after {:?} at {:?}",
                                                                 operator.lexeme,
                                                                 operator.position))));
            }
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
                let expr;
                {
                    if let Some(result) = parse_expression(tokens) {
                        expr = try_wrap_err!(result);
                    } else {
                        return Some(Err(ParseError::from_message(format!("Unfinished grouping \
                                                                     expression near {:?} at \
                                                                     {:?}",
                                                                         primary_token.lexeme,
                                                                         primary_token.position))));
                    }
                };
                {
                    if let Some(token) = tokens.next() {
                        if token.token == Token::RightParen {
                            let grouping_expression = Grouping { expr: expr };
                            return Some(Ok(Expr::Grouping(Box::new(grouping_expression))));
                        } else {
                            return Some(Err(ParseError::from_message(format!("Missing ) near \
                                                                              {:?} at {:?}",
                                                                             token.lexeme,
                                                                             token.position))));
                        }

                    }
                    return Some(Err(ParseError::from_message(format!("Missing ) before end of \
                                                                      file"))));
                }
            }
            _ => {
                return Some(Err(ParseError::from_message(format!("Unexpected token {:?} at \
                                                                  {:?}",
                                                                 primary_token.lexeme,
                                                                 primary_token.position))));
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
        let (tokens, _) = scan(&"123".into());
        let expr = parse(tokens).unwrap();
        assert_eq!("123", &expr.pretty_print());
    }

    #[test]
    fn binary() {
        let (tokens, _) = scan(&"123+456".into());
        let expr = parse(tokens).unwrap();
        assert_eq!("(+ 123 456)", &expr.pretty_print());
    }

    #[test]
    fn precedence_add_mul() {
        let (tokens, _) = scan(&"123+456*789".into());
        let expr = parse(tokens).unwrap();
        assert_eq!("(+ 123 (* 456 789))", &expr.pretty_print());
    }

    #[test]
    fn precedence_mul_add() {
        let (tokens, _) = scan(&"123*456+789".into());
        let expr = parse(tokens).unwrap();
        assert_eq!("(+ (* 123 456) 789)", &expr.pretty_print());
    }

    #[test]
    fn precedence_mul_add_unary() {
        let (tokens, _) = scan(&"-123*456+789".into());
        let expr = parse(tokens).unwrap();
        assert_eq!("(+ (* (- 123) 456) 789)", &expr.pretty_print());
    }

    #[test]
    fn unclosed_group() {
        let (tokens, _) = scan(&"(2".into());
        assert!(parse(tokens).is_err());
    }

    #[test]
    fn unopened_group() {
        let (tokens, _) = scan(&"2)".into());
        assert!(parse(tokens).is_err());
    }
}
