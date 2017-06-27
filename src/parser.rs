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
pub enum RequiredElement {
    Subexpression,
    LeftParen,
    ClosingParen,
    Semincolon,
    Identifier,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEndOfFile,
    UnexpectedToken(Lexeme, Position),
    Missing(RequiredElement, Lexeme, Position),
    InvalidAssignmentTarget(Lexeme, Position),
}

pub fn parse(tokens: &[TokenWithContext]) -> Result<Vec<Statement>, Vec<ParseError>> {
    let mut iter = tokens.iter().peekable();
    let mut statements = Vec::new();
    let mut errors = Vec::new();
    while let Some(result) = parse_declaration(&mut iter) {
        match result {
            Ok(statement) => {
                statements.push(statement);
            }
            Err(error) => {
                errors.push(error);
                synchronise(&mut iter);
            }
        }
    }
    if errors.is_empty() {
        Ok(statements)
    } else {
        Err(errors)
    }
}

fn synchronise<'a, I>(tokens: &mut Peekable<I>)
    where I: Iterator<Item = &'a TokenWithContext>
{
    enum PositionInConstruct {
        Start,
        Body,
        End,
    };
    fn classify(token: &Token) -> PositionInConstruct {
        match *token {
            Token::Semicolon => PositionInConstruct::End,
            Token::Class | Token::Fun | Token::Var | Token::For | Token::If | Token::While |
            Token::Print | Token::Return => PositionInConstruct::Start,
            _ => PositionInConstruct::Body,
        }
    }
    while let Some(token_kind) = tokens.peek().map(|t| classify(&t.token)) {
        match token_kind {
            PositionInConstruct::Start => {
                break;
            }
            PositionInConstruct::Body => {
                let _ = tokens.next();
            }
            PositionInConstruct::End => {
                let _ = tokens.next();
                break;
            }
        }
    }
}

fn parse_semicolon_terminated_statement<'a, I>(tokens: &mut Peekable<I>,
                                               parse_statement: &Fn(&mut Peekable<I>)
                                                                    -> Option<Result<Statement,
                                                                                      ParseError>>)
                                               -> Option<Result<Statement, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    match parse_statement(tokens) {
        Some(Ok(statement)) => {
            match tokens.peek() {
                Some(&&TokenWithContext { token: Token::Semicolon, .. }) => {
                    let _ = tokens.next();
                    Some(Ok(statement))
                }
                Some(&&TokenWithContext {
                         ref lexeme,
                         ref position,
                         ..
                     }) => {
                    Some(Err(ParseError::Missing(RequiredElement::Semincolon,
                                                 lexeme.clone(),
                                                 *position)))
                }
                None => Some(Err(ParseError::UnexpectedEndOfFile)),
            }
        }
        e => e,
    }
}

fn parse_declaration<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Statement, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    match tokens.peek().map(|t| &t.token) {
        Some(&Token::Var) => {
            let _ = tokens.next();
            parse_semicolon_terminated_statement(tokens, &parse_var_declaration)
        }
        Some(&Token::LeftBrace) => {
            let _ = tokens.next();
            parse_block(tokens)
        }
        Some(&Token::If) => {
            let _ = tokens.next();
            parse_if_statement(tokens)
        }
        Some(_) => parse_semicolon_terminated_statement(tokens, &parse_statement),
        None => None,
    }
}

fn parse_var_declaration<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Statement, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    let identifier = match tokens.peek() {
        Some(&&TokenWithContext { token: Token::Identifier(ref identifier), .. }) => {
            let _ = tokens.next();
            Identifier { name: identifier.clone() }
        }
        Some(&&TokenWithContext {
                 ref lexeme,
                 ref position,
                 ..
             }) => {
            return Some(Err(ParseError::Missing(RequiredElement::Identifier,
                                                lexeme.clone(),
                                                *position)))
        }
        None => return Some(Err(ParseError::UnexpectedEndOfFile)),
    };
    if let Some(&&TokenWithContext {
                    token: Token::Equal,
                    ref lexeme,
                    ref position,
                }) = tokens.peek() {
        let _ = tokens.next();
        match parse_expression(tokens) {
            Some(Ok(expression)) => {
                Some(Ok(Statement::VariableDefinitionWithInitalizer(identifier, expression)))
            }
            Some(Err(error)) => Some(Err(error)),
            None => {
                Some(Err(ParseError::Missing(RequiredElement::Subexpression,
                                             lexeme.clone(),
                                             *position)))
            }
        }
    } else {
        Some(Ok(Statement::VariableDefinition(identifier)))
    }
}

fn parse_block<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Statement, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    let mut statements = Vec::new();
    fn is_block_end(token: &&TokenWithContext) -> bool {
        match **token {
            TokenWithContext { token: Token::RightBrace, .. } => true,
            _ => false,
        }
    }
    while let Some(false) = tokens.peek().map(&is_block_end) {
        match parse_declaration(tokens) {
            Some(Ok(statement)) => statements.push(statement),
            None => return Some(Err(ParseError::UnexpectedEndOfFile)),
            Some(Err(error)) => return Some(Err(error)),
        }
    }
    if let Some(true) = tokens.peek().map(&is_block_end) {
        let _ = tokens.next();
        Some(Ok(Statement::Block(Box::new(Block { statements: statements }))))
    } else {
        Some(Err(ParseError::UnexpectedEndOfFile)) //TODO: better message
    }
}

fn parse_statement<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Statement, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    match tokens.peek().map(|t| &t.token) {
        Some(&Token::Print) => {
            let _ = tokens.next();
            parse_print_statement(tokens)
        }
        Some(_) => parse_expression_statement(tokens),
        None => None,
    }
}

fn parse_print_statement<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Statement, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    parse_expression(tokens).map(|r| r.map(Statement::Print))
}

fn parse_if_statement<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Statement, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    match tokens.peek().map(|t| &t.token) {
        Some(&Token::LeftParen) => {
            let _ = tokens.next();
        }
        Some(_) => {
            let token = tokens.next().unwrap();
            return Some(Err(ParseError::Missing(RequiredElement::LeftParen,
                                                token.lexeme.clone(),
                                                token.position)));
        }
        None => return Some(Err(ParseError::UnexpectedEndOfFile)),
    }
    let condition = match parse_expression(tokens) {
        Some(Ok(expression)) => expression,
        Some(Err(error)) => return Some(Err(error)),
        None => return Some(Err(ParseError::UnexpectedEndOfFile)),
    };
    match tokens.peek().map(|t| &t.token) {
        Some(&Token::RightParen) => {
            let _ = tokens.next();
        }
        Some(_) => {
            let token = tokens.next().unwrap();
            return Some(Err(ParseError::Missing(RequiredElement::ClosingParen,
                                                token.lexeme.clone(),
                                                token.position)));
        }
        None => return Some(Err(ParseError::UnexpectedEndOfFile)),
    }
    // I'd rather use parse_block instead of parse_declaration
    // that would require the presence of the brackets
    let then_branch = match parse_declaration(tokens) {
        Some(Ok(statement)) => statement,
        Some(Err(error)) => return Some(Err(error)),
        None => return Some(Err(ParseError::UnexpectedEndOfFile)),
    };
    if let Some(&Token::Else) = tokens.peek().map(|t| &t.token) {
        let _ = tokens.next();
        let else_branch = match parse_declaration(tokens) {
            Some(Ok(statement)) => statement,
            Some(Err(error)) => return Some(Err(error)),
            None => return Some(Err(ParseError::UnexpectedEndOfFile)),
        };
        Some(Ok(Statement::IfThenElse(Box::new(IfThenElse {
                                                   condition: condition,
                                                   then_branch: then_branch,
                                                   else_branch: else_branch,
                                               }))))
    } else {
        Some(Ok(Statement::IfThen(Box::new(IfThen {
                                               condition: condition,
                                               then_branch: then_branch,
                                           }))))
    }
}

fn parse_expression_statement<'a, I>(tokens: &mut Peekable<I>)
                                     -> Option<Result<Statement, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    parse_expression(tokens).map(|r| r.map(Statement::Expression))
}

fn parse_expression<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    parse_assignment(tokens)
}

fn parse_logic<'a, I>(tokens: &mut Peekable<I>,
                      map_operator: &Fn(&Token) -> Option<LogicOperator>,
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
        let right = if let Some(result) = parse_subexpression(tokens) {
            try_wrap_err!(result)
        } else {
            return Some(Err(ParseError::Missing(RequiredElement::Subexpression,
                                                operator.lexeme.clone(),
                                                operator.position)));
        };
        let binary_expression = LogicExpr {
            left: expr,
            operator: mapped_operator,
            right: right,
        };
        expr = Expr::Logic(Box::new(binary_expression));
    }
    Some(Ok(expr))
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
        let right = if let Some(result) = parse_subexpression(tokens) {
            try_wrap_err!(result)
        } else {
            return Some(Err(ParseError::Missing(RequiredElement::Subexpression,
                                                operator.lexeme.clone(),
                                                operator.position)));
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

fn parse_assignment<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    match parse_or(tokens) {
        Some(Ok(lvalue)) => {
            if let Some(&Token::Equal) = tokens.peek().map(|t| &t.token) {
                let equal = tokens.next().unwrap();
                match lvalue {
                    Expr::Identifier(identifier) => {
                        let target = Target::Identifier(Identifier { name: identifier.name });
                        match parse_assignment(tokens) {
                            None => Some(Err(ParseError::UnexpectedEndOfFile)),
                            Some(result) => {
                                Some(result.map(|rvalue| {
                                                    Expr::Assignment(Box::new(Assignment {
                                                                                  lvalue: target,
                                                                                  rvalue: rvalue,
                                                                              }))
                                                }))
                            }
                        }
                    }
                    _ => {
                        Some(Err(ParseError::InvalidAssignmentTarget(equal.lexeme.clone(),
                                                                     equal.position)))
                    }
                }
            } else {
                Some(Ok(lvalue))
            }
        }
        result => result,
    }
}

fn parse_or<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<LogicOperator> {
        match *token {
            Token::Or => Some(LogicOperator::Or),
            _ => None,
        }
    }
    parse_logic(tokens, &map_operator, &parse_and)
}

fn parse_and<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<LogicOperator> {
        match *token {
            Token::And => Some(LogicOperator::And),
            _ => None,
        }
    }
    parse_logic(tokens, &map_operator, &parse_equality)
}

fn parse_equality<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<BinaryOperator> {
        match *token {
            Token::BangEqual => Some(BinaryOperator::NotEqual),
            Token::EqualEqual => Some(BinaryOperator::Equal),
            _ => None,
        }
    }
    parse_binary(tokens, &map_operator, &parse_comparison)
}

fn parse_comparison<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<BinaryOperator> {
        match *token {
            Token::Greater => Some(BinaryOperator::Greater),
            Token::GreaterEqual => Some(BinaryOperator::GreaterEqual),
            Token::Less => Some(BinaryOperator::Less),
            Token::LessEqual => Some(BinaryOperator::LessEqual),
            _ => None,
        }
    }
    parse_binary(tokens, &map_operator, &parse_term)
}

fn parse_term<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<BinaryOperator> {
        match *token {
            Token::Minus => Some(BinaryOperator::Minus),
            Token::Plus => Some(BinaryOperator::Plus),
            _ => None,
        }
    }
    parse_binary(tokens, &map_operator, &parse_factor)
}

fn parse_factor<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<BinaryOperator> {
        match *token {
            Token::Slash => Some(BinaryOperator::Slash),
            Token::Star => Some(BinaryOperator::Star),
            _ => None,
        }
    }
    parse_binary(tokens, &map_operator, &parse_unary)
}

fn parse_unary<'a, I>(tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn map_operator(token: &Token) -> Option<UnaryOperator> {
        match *token {
            Token::Minus => Some(UnaryOperator::Minus),
            Token::Bang => Some(UnaryOperator::Bang),
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
            return Some(Err(ParseError::Missing(RequiredElement::Subexpression,
                                                operator.lexeme.clone(),
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
            Token::Identifier(ref i) => Expr::Identifier(Identifier { name: i.clone() }),
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
                            return Some(Err(ParseError::Missing(RequiredElement::ClosingParen,
                                                                token.lexeme.clone(),
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
        let expr = parse_expression(&mut tokens.iter().peekable())
            .unwrap()
            .unwrap();
        assert_eq!(&string, &expr.pretty_print());
    }

    #[test]
    fn binary() {
        let (tokens, _) = scan(&"123+456");
        let expr = parse_expression(&mut tokens.iter().peekable())
            .unwrap()
            .unwrap();
        assert_eq!("(+ 123 456)", &expr.pretty_print());
    }

    #[test]
    fn precedence_add_mul() {
        let (tokens, _) = scan(&"123+456*789");
        let expr = parse_expression(&mut tokens.iter().peekable())
            .unwrap()
            .unwrap();
        assert_eq!("(+ 123 (* 456 789))", &expr.pretty_print());
    }

    #[test]
    fn precedence_mul_add() {
        let (tokens, _) = scan(&"123*456+789");
        let expr = parse_expression(&mut tokens.iter().peekable())
            .unwrap()
            .unwrap();
        assert_eq!("(+ (* 123 456) 789)", &expr.pretty_print());
    }

    #[test]
    fn precedence_mul_add_unary() {
        let (tokens, _) = scan(&"-123*456+789");
        let expr = parse_expression(&mut tokens.iter().peekable())
            .unwrap()
            .unwrap();
        assert_eq!("(+ (* (- 123) 456) 789)", &expr.pretty_print());
    }

    #[test]
    fn logic() {
        let (tokens, _) = scan(&"123 and 456");
        let expr = parse_expression(&mut tokens.iter().peekable())
            .unwrap()
            .unwrap();
        assert_eq!("(and 123 456)", &expr.pretty_print());
    }

    #[test]
    fn precedence_or_and() {
        let (tokens, _) = scan(&"a or b and c");
        let expr = parse_expression(&mut tokens.iter().peekable())
            .unwrap()
            .unwrap();
        assert_eq!("(or a (and b c))", &expr.pretty_print());
    }

    #[test]
    fn unclosed_group() {
        let (tokens, _) = scan(&"(2");
        assert!(parse(&tokens).is_err());
    }

    #[test]
    fn unopened_group() {
        let (tokens, _) = scan(&"2)");
        assert!(parse(&tokens).is_err());
    }

    #[test]
    fn multiple_statements() {
        let (tokens, _) = scan(&"var x; {var x=10; print x;} print x;");
        let statements = parse(&tokens).unwrap();
        assert_eq!("var x;", statements[0].pretty_print());
        assert_eq!("{ var x = 10; print x; }", statements[1].pretty_print());
        assert_eq!("print x;", statements[2].pretty_print());
    }

    #[test]
    fn expr_with_equality() {
        let (tokens, _) = scan(&"x/2 == 1;");
        let statements = parse(&tokens).unwrap();
        assert_eq!("(== (/ x 2) 1);", statements[0].pretty_print());
    }

    #[test]
    fn assignment() {
        let (tokens, _) = scan(&"a = 1;");
        let statements = parse(&tokens).unwrap();
        assert_eq!("a = 1;", statements[0].pretty_print());
    }

    #[test]
    fn wrong_variable_declaration_target() {
        let (tokens, _) = scan(&"var 1 = 1;");
        assert!(parse(&tokens).is_err());
    }

    #[test]
    fn wrong_assigment_target() {
        let (tokens, _) = scan(&"1 = 1;");
        assert!(parse(&tokens).is_err());
    }

    #[test]
    fn missing_initializer() {
        let (tokens, _) = scan(&"var a =;");
        assert!(parse(&tokens).is_err());
    }

    #[test]
    fn unfinished_block() {
        let (tokens, _) = scan(&"{var a = 1;");
        assert!(parse(&tokens).is_err());
    }

    #[test]
    fn if_then_statement() {
        let (tokens, _) = scan(&"if(a) x = 2;");
        let statements = parse(&tokens).unwrap();
        assert_eq!("if ( a ) x = 2;", statements[0].pretty_print());
    }

    #[test]
    fn if_then_else_statement() {
        let (tokens, _) = scan(&"if(a and b) { x = 2;} else{x = 3;}");
        let statements = parse(&tokens).unwrap();
        assert_eq!("if ( (and a b) ) { x = 2; } else { x = 3; }",
                   statements[0].pretty_print());
    }
}
