use ast::*;
use scanner::{Lexeme, Position, Token, TokenWithContext};
use std::iter::Peekable;
use std::rc::Rc;

/// This behave exactly as try! but wraps the returned result in a Some.
/// It's useful to remove some boilerplate in the code introduced by
/// the use of Option<Result<T, E>>
macro_rules! try_wrap_err {
    ($e:expr) => (match $e {Ok(e) => e, Err(e) => return Some(Err(e))})
}

macro_rules! consume_expected_token_with_action {
    ($tokens:expr, $expected: pat, $transform_token: expr, $required_element: expr) => (
    match $tokens.peek().map(|t| &t.token) {
        Some($expected) => {
            let _ = $tokens.next();
            Ok($transform_token)
        }
        Some(_) => {
            let token = $tokens.next().unwrap();
            Err(
                ParseError::Missing(
                    $required_element,
                    token.lexeme.clone(),
                    token.position))
        }
        None => Err(ParseError::UnexpectedEndOfFile),
    }
    )
}

macro_rules! consume_expected_token {
    ($tokens:expr, $expected: pat, $required_element: expr) => (
        consume_expected_token_with_action!($tokens, $expected, (), $required_element)
    )
}

#[derive(Debug)]
pub enum RequiredElement {
    Subexpression,
    LeftParen,
    RightParen,
    Semicolon,
    Identifier,
    Block,
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedEndOfFile,
    UnexpectedToken(Lexeme, Position),
    Missing(RequiredElement, Lexeme, Position),
    InvalidAssignmentTarget(Lexeme, Position),
    TooManyArguments,
}

pub fn parse(tokens: &[TokenWithContext]) -> Result<Vec<Statement>, Vec<ParseError>> {
    let mut statements = Vec::new();
    let mut errors = Vec::new();
    let mut parser = Parser::new(tokens.iter().peekable());
    while let Some(result) = parser.parse_declaration() {
        match result {
            Ok(statement) => {
                statements.push(statement);
            }
            Err(error) => {
                errors.push(error);
                parser.synchronise();
            }
        }
    }
    if errors.is_empty() {
        Ok(statements)
    } else {
        Err(errors)
    }
}

pub struct Parser<'a, I>
    where I: Iterator<Item = &'a TokenWithContext>
{
    tokens: Peekable<I>,
    expression_handle_factory: ExpressionHandleFactory,
}

impl<'a, I> Parser<'a, I>
    where I: Iterator<Item = &'a TokenWithContext>
{
    fn new(tokens: Peekable<I>) -> Parser<'a, I> {
        Parser {
            tokens: tokens,
            expression_handle_factory: ExpressionHandleFactory::new(),
        }
    }

    fn consume_expected_identifier(&mut self) -> Result<Identifier, ParseError> {
        consume_expected_token_with_action!(self.tokens,
    &Token::Identifier(ref identifier),
    Identifier { name: identifier.clone() },
    RequiredElement::Identifier)
    }

    fn synchronise(&mut self) {
        enum PositionInConstruct {
            Start,
            Body,
            End,
        };
        fn classify(token: &Token) -> PositionInConstruct {
            match *token {
                Token::Semicolon => PositionInConstruct::End,
                Token::Class | Token::Fun | Token::Var | Token::For | Token::If |
                Token::While | Token::Print | Token::Return => PositionInConstruct::Start,
                _ => PositionInConstruct::Body,
            }
        }
        while let Some(token_kind) = self.tokens.peek().map(|t| classify(&t.token)) {
            match token_kind {
                PositionInConstruct::Start => {
                    break;
                }
                PositionInConstruct::Body => {
                    let _ = self.tokens.next();
                }
                PositionInConstruct::End => {
                    let _ = self.tokens.next();
                    break;
                }
            }
        }
    }

    fn parse_semicolon_terminated_statement(&mut self,
                                            parse_statement: &Fn(&mut Parser<'a, I>)
                                                                 -> Option<Result<Statement,
                                                                                   ParseError>>)
                                            -> Option<Result<Statement, ParseError>> {
        match parse_statement(self) {
            Some(Ok(statement)) => {
                match self.tokens.peek() {
                    Some(&&TokenWithContext { token: Token::Semicolon, .. }) => {
                        let _ = self.tokens.next();
                        Some(Ok(statement))
                    }
                    Some(&&TokenWithContext {
                             ref lexeme,
                             ref position,
                             ..
                         }) => {
                        Some(Err(ParseError::Missing(RequiredElement::Semicolon,
                                                     lexeme.clone(),
                                                     *position)))
                    }
                    None => Some(Err(ParseError::UnexpectedEndOfFile)),
                }
            }
            e => e,
        }
    }

    fn parse_declaration(&mut self) -> Option<Result<Statement, ParseError>> {
        match self.tokens.peek().map(|t| &t.token) {
            Some(&Token::Var) => {
                let _ = self.tokens.next();
                self.parse_semicolon_terminated_statement(&Parser::parse_var_declaration)
            }
            Some(&Token::LeftBrace) => {
                let _ = self.tokens.next();
                self.parse_block()
            }
            Some(&Token::Fun) => {
                let _ = self.tokens.next();
                self.parse_function_declaration()
            }
            Some(&Token::If) => {
                let _ = self.tokens.next();
                self.parse_if_statement()
            }
            Some(&Token::While) => {
                let _ = self.tokens.next();
                self.parse_while_statement()
            }
            Some(&Token::For) => {
                let _ = self.tokens.next();
                self.parse_for_statement()
            }
            Some(_) => self.parse_semicolon_terminated_statement(&Parser::parse_statement),
            None => None,
        }
    }

    fn parse_function_declaration(&mut self) -> Option<Result<Statement, ParseError>> {
        let identifier = try_wrap_err!(self.consume_expected_identifier());
        let parse_identifier =
            |parser: &mut Parser<'a, I>| Some(parser.consume_expected_identifier());
        let arguments = try_wrap_err!(self.parse_function_arguments(&parse_identifier));
        let _ =
            try_wrap_err!(
                consume_expected_token!(self.tokens, &Token::LeftBrace, RequiredElement::Block));
        let block = match self.parse_block() {
            Some(Ok(block)) => block,
            Some(err) => return Some(err),
            None => return Some(Err(ParseError::UnexpectedEndOfFile)),
        };
        Some(Ok(Statement::FunctionDefinition(Rc::new(FunctionDefinition {
                                                          name: identifier,
                                                          arguments: arguments,
                                                          body: block,
                                                      }))))
    }


    fn parse_var_declaration(&mut self) -> Option<Result<Statement, ParseError>> {
        let identifier = try_wrap_err!(self.consume_expected_identifier());
        if let Some(&&TokenWithContext {
                        token: Token::Equal,
                        ref lexeme,
                        ref position,
                    }) = self.tokens.peek() {
            let _ = self.tokens.next();
            match self.parse_expression() {
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

    fn parse_block(&mut self) -> Option<Result<Statement, ParseError>> {
        let mut statements = Vec::new();
        fn is_block_end(token: &&TokenWithContext) -> bool {
            match **token {
                TokenWithContext { token: Token::RightBrace, .. } => true,
                _ => false,
            }
        }
        while let Some(false) = self.tokens.peek().map(&is_block_end) {
            match self.parse_declaration() {
                Some(Ok(statement)) => statements.push(statement),
                None => return Some(Err(ParseError::UnexpectedEndOfFile)),
                Some(Err(error)) => return Some(Err(error)),
            }
        }
        if let Some(true) = self.tokens.peek().map(&is_block_end) {
            let _ = self.tokens.next();
            Some(Ok(Statement::Block(Box::new(Block { statements: statements }))))
        } else {
            Some(Err(ParseError::UnexpectedEndOfFile)) //TODO: better message
        }
    }

    fn parse_statement(&mut self) -> Option<Result<Statement, ParseError>> {
        match self.tokens.peek().map(|t| &t.token) {
            Some(&Token::Print) => {
                let _ = self.tokens.next();
                self.parse_print_statement()
            }
            Some(&Token::Return) => {
                let _ = self.tokens.next();
                self.parse_return_statement()
            }
            Some(_) => self.parse_expression_statement(),
            None => None,
        }
    }

    fn parse_print_statement(&mut self) -> Option<Result<Statement, ParseError>> {
        self.parse_expression().map(|r| r.map(Statement::Print))
    }

    fn parse_return_statement(&mut self) -> Option<Result<Statement, ParseError>> {
        if let Some(&Token::Semicolon) = self.tokens.peek().map(|t| &t.token) {
            Some(Ok(Statement::Return(None)))
        } else {
            self.parse_expression()
                .map(|result| result.map(|expression| Statement::Return(Some(expression))))
        }
    }

    fn parse_if_statement(&mut self) -> Option<Result<Statement, ParseError>> {
        let _ = try_wrap_err!(
                consume_expected_token!(
                    self.tokens, &Token::LeftParen, RequiredElement::LeftParen));
        let condition = match self.parse_expression() {
            Some(Ok(expression)) => expression,
            Some(Err(error)) => return Some(Err(error)),
            None => return Some(Err(ParseError::UnexpectedEndOfFile)),
        };
        let _ = try_wrap_err!(
            consume_expected_token!(
                self.tokens, &Token::RightParen, RequiredElement::RightParen));
        // I'd rather use parse_block instead of parse_declaration
        // that would require the presence of the brackets
        let then_branch = match self.parse_declaration() {
            Some(Ok(statement)) => statement,
            Some(Err(error)) => return Some(Err(error)),
            None => return Some(Err(ParseError::UnexpectedEndOfFile)),
        };
        if let Some(&Token::Else) = self.tokens.peek().map(|t| &t.token) {
            let _ = self.tokens.next();
            let else_branch = match self.parse_declaration() {
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

    fn parse_while_statement(&mut self) -> Option<Result<Statement, ParseError>> {
        let _ =
            try_wrap_err!(
            consume_expected_token!(self.tokens, &Token::LeftParen, RequiredElement::LeftParen));
        let condition = match self.parse_expression() {
            Some(Ok(expression)) => expression,
            Some(Err(error)) => return Some(Err(error)),
            None => return Some(Err(ParseError::UnexpectedEndOfFile)),
        };
        let _ =
            try_wrap_err!(
            consume_expected_token!(self.tokens, &Token::RightParen, RequiredElement::RightParen));
        // I'd rather use parse_block instead of parse_declaration
        // that would require the presence of the brackets
        let body = match self.parse_declaration() {
            Some(Ok(statement)) => statement,
            Some(Err(error)) => return Some(Err(error)),
            None => return Some(Err(ParseError::UnexpectedEndOfFile)),
        };
        Some(Ok(Statement::While(Box::new(While {
                                              condition: condition,
                                              body: body,
                                          }))))
    }

    fn parse_for_statement(&mut self) -> Option<Result<Statement, ParseError>> {
        let _ =
            try_wrap_err!(
            consume_expected_token!(self.tokens, &Token::LeftParen, RequiredElement::LeftParen));
        let initializer = match self.tokens.peek().map(|t| &t.token) {
            Some(&Token::Semicolon) => None,
            Some(&Token::Var) => {
                let _ = self.tokens.next();
                match self.parse_var_declaration() {
                    Some(Ok(expression)) => Some(expression),
                    Some(Err(error)) => return Some(Err(error)),
                    None => return Some(Err(ParseError::UnexpectedEndOfFile)),
                }
            }
            _ => {
                match self.parse_expression_statement() {
                    Some(Ok(expression)) => Some(expression),
                    Some(Err(error)) => return Some(Err(error)),
                    None => return Some(Err(ParseError::UnexpectedEndOfFile)),
                }
            }
        };
        let _ =
            try_wrap_err!(
            consume_expected_token!(self.tokens, &Token::Semicolon, RequiredElement::Semicolon));

        let condition = match self.tokens.peek().map(|t| &t.token) {
            Some(&Token::Semicolon) => {
                Expr {
                    handle: self.expression_handle_factory.next(),
                    expr: ExprEnum::Literal(Literal::BoolLiteral(true)),
                }
            }
            _ => {
                match self.parse_expression() {
                    Some(Ok(expression)) => expression,
                    Some(Err(error)) => return Some(Err(error)),
                    None => return Some(Err(ParseError::UnexpectedEndOfFile)),
                }
            }
        };

        let _ =
            try_wrap_err!(
            consume_expected_token!(self.tokens, &Token::Semicolon, RequiredElement::Semicolon));

        let increment = match self.tokens.peek().map(|t| &t.token) {
            Some(&Token::RightParen) => None,
            _ => {
                match self.parse_expression() {
                    Some(Ok(expression)) => Some(expression),
                    Some(Err(error)) => return Some(Err(error)),
                    None => return Some(Err(ParseError::UnexpectedEndOfFile)),
                }
            }
        };
        let _ =
            try_wrap_err!(
            consume_expected_token!(self.tokens, &Token::RightParen, RequiredElement::RightParen));

        // I'd rather use parse_block instead of parse_declaration
        // that would require the presence of the brackets
        let body = match self.parse_declaration() {
            Some(Ok(statement)) => statement,
            Some(Err(error)) => return Some(Err(error)),
            None => return Some(Err(ParseError::UnexpectedEndOfFile)),
        };
        // Desugaring
        let desugared_body = if let Some(increment_expression) = increment {
            let desugared_statements = vec![body, Statement::Expression(increment_expression)];
            Statement::Block(Box::new(Block { statements: desugared_statements }))
        } else {
            body
        };
        let while_statement = Statement::While(Box::new(While {
                                                            condition: condition,
                                                            body: desugared_body,
                                                        }));
        Some(Ok(if let Some(initializer) = initializer {
                    let desugared_statements = vec![initializer, while_statement];
                    Statement::Block(Box::new(Block { statements: desugared_statements }))
                } else {
                    while_statement
                }))
    }

    fn parse_expression_statement(&mut self) -> Option<Result<Statement, ParseError>> {
        self.parse_expression()
            .map(|r| r.map(Statement::Expression))
    }

    fn parse_expression(&mut self) -> Option<Result<Expr, ParseError>> {
        self.parse_assignment()
    }

    fn parse_logic(&mut self,
                   map_operator: &Fn(&Token) -> Option<LogicOperator>,
                   parse_subexpression: &Fn(&mut Parser<'a, I>) -> Option<Result<Expr, ParseError>>)
                   -> Option<Result<Expr, ParseError>> {
        let mut expr;
        {
            let result = parse_subexpression(self);
            if let Some(Ok(subexpression)) = result {
                expr = subexpression;
            } else {
                return result;
            }
        };
        while let Some(Some(mapped_operator)) =
            self.tokens.peek().map(|pt| map_operator(&pt.token)) {
            let operator; // We know it's right, we read it just for error reporting
            {
                operator = self.tokens.next().unwrap();
            }
            let right = if let Some(result) = parse_subexpression(self) {
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
            expr = Expr {
                handle: self.expression_handle_factory.next(),
                expr: ExprEnum::Logic(Box::new(binary_expression)),
            };
        }
        Some(Ok(expr))
    }

    fn parse_binary(&mut self,
                    map_operator: &Fn(&Token) -> Option<BinaryOperator>,
                    parse_subexpression: &Fn(&mut Parser<'a, I>)
                                             -> Option<Result<Expr, ParseError>>)
                    -> Option<Result<Expr, ParseError>> {
        let mut expr;
        {
            let result = parse_subexpression(self);
            if let Some(Ok(subexpression)) = result {
                expr = subexpression;
            } else {
                return result;
            }
        };
        while let Some(Some(mapped_operator)) =
            self.tokens.peek().map(|pt| map_operator(&pt.token)) {
            let operator; // We know it's right, we read it just for error reporting
            {
                operator = self.tokens.next().unwrap();
            }
            let right = if let Some(result) = parse_subexpression(self) {
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
            expr = Expr {
                handle: self.expression_handle_factory.next(),
                expr: ExprEnum::Binary(Box::new(binary_expression)),
            };
        }
        Some(Ok(expr))
    }

    fn parse_assignment(&mut self) -> Option<Result<Expr, ParseError>> {
        match self.parse_or() {
            Some(Ok(lvalue)) => {
                if let Some(&Token::Equal) = self.tokens.peek().map(|t| &t.token) {
                    let equal = self.tokens.next().unwrap();
                    match lvalue {
                        Expr {
                            handle: _,
                            expr: ExprEnum::Identifier(identifier),
                        } => {
                            let target = Target::Identifier(Identifier { name: identifier.name });
                            match self.parse_assignment() {
                                None => Some(Err(ParseError::UnexpectedEndOfFile)),
                                Some(result) => {
                                    Some(result.map(|rvalue| {
                                        Expr {
                                            handle: self.expression_handle_factory.next(),
                                            expr: ExprEnum::Assignment(Box::new(Assignment {
                                                                                    lvalue: target,
                                                                                    rvalue: rvalue,
                                                                                })),
                                        }
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

    fn parse_or(&mut self) -> Option<Result<Expr, ParseError>> {
        fn map_operator(token: &Token) -> Option<LogicOperator> {
            match *token {
                Token::Or => Some(LogicOperator::Or),
                _ => None,
            }
        }
        self.parse_logic(&map_operator, &Parser::parse_and)
    }

    fn parse_and(&mut self) -> Option<Result<Expr, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        fn map_operator(token: &Token) -> Option<LogicOperator> {
            match *token {
                Token::And => Some(LogicOperator::And),
                _ => None,
            }
        }
        self.parse_logic(&map_operator, &Parser::parse_equality)
    }

    fn parse_equality(&mut self) -> Option<Result<Expr, ParseError>> {
        fn map_operator(token: &Token) -> Option<BinaryOperator> {
            match *token {
                Token::BangEqual => Some(BinaryOperator::NotEqual),
                Token::EqualEqual => Some(BinaryOperator::Equal),
                _ => None,
            }
        }
        self.parse_binary(&map_operator, &Parser::parse_comparison)
    }

    fn parse_comparison(&mut self) -> Option<Result<Expr, ParseError>> {
        fn map_operator(token: &Token) -> Option<BinaryOperator> {
            match *token {
                Token::Greater => Some(BinaryOperator::Greater),
                Token::GreaterEqual => Some(BinaryOperator::GreaterEqual),
                Token::Less => Some(BinaryOperator::Less),
                Token::LessEqual => Some(BinaryOperator::LessEqual),
                _ => None,
            }
        }
        self.parse_binary(&map_operator, &Parser::parse_term)
    }

    fn parse_term(&mut self) -> Option<Result<Expr, ParseError>> {
        fn map_operator(token: &Token) -> Option<BinaryOperator> {
            match *token {
                Token::Minus => Some(BinaryOperator::Minus),
                Token::Plus => Some(BinaryOperator::Plus),
                _ => None,
            }
        }
        self.parse_binary(&map_operator, &Parser::parse_factor)
    }

    fn parse_factor(&mut self) -> Option<Result<Expr, ParseError>> {
        fn map_operator(token: &Token) -> Option<BinaryOperator> {
            match *token {
                Token::Slash => Some(BinaryOperator::Slash),
                Token::Star => Some(BinaryOperator::Star),
                _ => None,
            }
        }
        self.parse_binary(&map_operator, &Parser::parse_unary)
    }

    fn parse_unary(&mut self) -> Option<Result<Expr, ParseError>> {
        fn map_operator(token: &Token) -> Option<UnaryOperator> {
            match *token {
                Token::Minus => Some(UnaryOperator::Minus),
                Token::Bang => Some(UnaryOperator::Bang),
                _ => None,
            }
        }
        if let Some(Some(mapped_operator)) = self.tokens.peek().map(|pt| map_operator(&pt.token)) {
            let operator; // For error reporting
            {
                operator = self.tokens.next().unwrap();
            }
            let right = if let Some(result) = self.parse_unary() {
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
            return Some(Ok(Expr {
                               handle: self.expression_handle_factory.next(),
                               expr: ExprEnum::Unary(Box::new(unary_expression)),
                           }));
        } else {
            self.parse_call()
        }
    }

    fn parse_call(&mut self) -> Option<Result<Expr, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        let mut expression = match self.parse_primary() {
            Some(Ok(expression)) => expression,
            error => return error,
        };
        while let Some(&Token::LeftParen) = self.tokens.peek().map(|t| &t.token) {
            expression = match self.finish_call(expression) {
                Some(Ok(expression)) => expression,
                error => return error,
            };
        }
        Some(Ok(expression))
    }

    fn parse_function_arguments<A>(&mut self,
                                   parse_argument: &Fn(&mut Parser<'a, I>)
                                                       -> Option<Result<A, ParseError>>)
                                   -> Result<Vec<A>, ParseError> {
        let _ = try!(consume_expected_token!(self.tokens, &Token::LeftParen, RequiredElement::LeftParen));
        let mut arguments = vec![];
        if let Some(&Token::RightParen) = self.tokens.peek().map(|t| &t.token) {
        } else {
            loop {
                match parse_argument(self) {
                    Some(Ok(expression)) => {
                        if arguments.len() >= 8 {
                            return Err(ParseError::TooManyArguments);
                        }
                        arguments.push(expression)
                    }
                    Some(Err(error)) => return Err(error),
                    None => return Err(ParseError::UnexpectedEndOfFile),
                };
                if let Some(&Token::Comma) = self.tokens.peek().map(|t| &t.token) {
                    let _ = self.tokens.next();
                } else {
                    break;
                }
            }
        }
        let _ =
            try!(
            consume_expected_token!(self.tokens, &Token::RightParen, RequiredElement::RightParen));
        Ok(arguments)
    }

    fn finish_call(&mut self, callee: Expr) -> Option<Result<Expr, ParseError>> {
        let arguments = try_wrap_err!(self.parse_function_arguments(&Parser::parse_expression));
        Some(Ok(Expr {
                    handle: self.expression_handle_factory.next(),
                    expr: ExprEnum::Call(Box::new(Call {
                                                      callee: callee,
                                                      arguments: arguments,
                                                  })),
                }))
    }

    fn parse_primary(&mut self) -> Option<Result<Expr, ParseError>> {
        let primary_token;
        {
            primary_token = self.tokens.next();
        };
        if let Some(primary_token) = primary_token {
            let parsed_expression = match primary_token.token {
                Token::False => {
                    Expr {
                        handle: self.expression_handle_factory.next(),
                        expr: ExprEnum::Literal(Literal::BoolLiteral(false)),
                    }
                }
                Token::True => {
                    Expr {
                        handle: self.expression_handle_factory.next(),
                        expr: ExprEnum::Literal(Literal::BoolLiteral(true)),
                    }
                }
                Token::Nil => {
                    Expr {
                        handle: self.expression_handle_factory.next(),
                        expr: ExprEnum::Literal(Literal::NilLiteral),
                    }
                }
                Token::NumberLiteral(n) => {
                    Expr {
                        handle: self.expression_handle_factory.next(),
                        expr: ExprEnum::Literal(Literal::NumberLiteral(n)),
                    }
                }
                Token::StringLiteral(ref s) => {
                    Expr {
                        handle: self.expression_handle_factory.next(),
                        expr: ExprEnum::Literal(Literal::StringLiteral(s.clone())),
                    }
                }
                Token::Identifier(ref i) => {
                    Expr {
                        handle: self.expression_handle_factory.next(),
                        expr: ExprEnum::Identifier(Identifier { name: i.clone() }),
                    }
                }
                Token::LeftParen => {
                    let expr = if let Some(result) = self.parse_expression() {
                        try_wrap_err!(result)
                    } else {
                        return Some(Err(ParseError::UnexpectedEndOfFile));
                    };
                    {
                        if let Some(token) = self.tokens.next() {
                            if token.token == Token::RightParen {
                                let grouping_expression = Grouping { expr: expr };
                                return Some(Ok(
                                    Expr{
                                    handle: self.expression_handle_factory.next(),
                expr: ExprEnum::Grouping(Box::new(grouping_expression))}));
                            } else {
                                return Some(Err(ParseError::Missing(RequiredElement::RightParen,
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
        let mut parser = Parser::new(tokens.iter().peekable());
        let expr = parser.parse_expression().unwrap().unwrap();
        assert_eq!(&string, &expr.pretty_print());
    }

    #[test]
    fn binary() {
        let (tokens, _) = scan(&"123+456");
        let mut parser = Parser::new(tokens.iter().peekable());
        let expr = parser.parse_expression().unwrap().unwrap();
        assert_eq!("(+ 123 456)", &expr.pretty_print());
    }

    #[test]
    fn precedence_add_mul() {
        let (tokens, _) = scan(&"123+456*789");
        let mut parser = Parser::new(tokens.iter().peekable());
        let expr = parser.parse_expression().unwrap().unwrap();
        assert_eq!("(+ 123 (* 456 789))", &expr.pretty_print());
    }

    #[test]
    fn precedence_mul_add() {
        let (tokens, _) = scan(&"123*456+789");
        let mut parser = Parser::new(tokens.iter().peekable());
        let expr = parser.parse_expression().unwrap().unwrap();
        assert_eq!("(+ (* 123 456) 789)", &expr.pretty_print());
    }

    #[test]
    fn precedence_mul_add_unary() {
        let (tokens, _) = scan(&"-123*456+789");
        let mut parser = Parser::new(tokens.iter().peekable());
        let expr = parser.parse_expression().unwrap().unwrap();
        assert_eq!("(+ (* (- 123) 456) 789)", &expr.pretty_print());
    }

    #[test]
    fn logic() {
        let (tokens, _) = scan(&"123 and 456");
        let mut parser = Parser::new(tokens.iter().peekable());
        let expr = parser.parse_expression().unwrap().unwrap();
        assert_eq!("(and 123 456)", &expr.pretty_print());
    }

    #[test]
    fn precedence_or_and() {
        let (tokens, _) = scan(&"a or b and c");
        let mut parser = Parser::new(tokens.iter().peekable());
        let expr = parser.parse_expression().unwrap().unwrap();
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

    #[test]
    fn while_statement() {
        let (tokens, _) = scan(&"while(a > 0){ a = a - 1;}");
        let statements = parse(&tokens).unwrap();
        assert_eq!("while ( (> a 0) ) { a = (- a 1); }",
                   statements[0].pretty_print());
    }

    #[test]
    fn for_statement() {
        let (tokens, _) = scan(&"for (var i = 0; i < 10; i = i + 1) print i;");
        let statements = parse(&tokens).unwrap();
        assert_eq!("{ var i = 0; while ( (< i 10) ) { print i; i = (+ i 1); } }",
                   statements[0].pretty_print());
    }
    #[test]
    fn for_statement_no_initializer() {
        let (tokens, _) = scan(&"for (; i < 10; i = i + 1) print i;");
        let statements = parse(&tokens).unwrap();
        assert_eq!("while ( (< i 10) ) { print i; i = (+ i 1); }",
                   statements[0].pretty_print());
    }
    #[test]
    fn for_statement_no_condition() {
        let (tokens, _) = scan(&"for (var i = 0;; i = i + 1) print i;");
        let statements = parse(&tokens).unwrap();
        assert_eq!("{ var i = 0; while ( true ) { print i; i = (+ i 1); } }",
                   statements[0].pretty_print());
    }

    #[test]
    fn for_statement_no_increment() {
        let (tokens, _) = scan(&"for (var i = 0; i < 10;) print i;");
        let statements = parse(&tokens).unwrap();
        assert_eq!("{ var i = 0; while ( (< i 10) ) print i; }",
                   statements[0].pretty_print());
    }

    #[test]
    fn function_with_no_arguments() {
        let (tokens, _) = scan(&"fun add(){return 1;}");
        let statements = parse(&tokens).unwrap();
        assert_eq!("fun add () { return 1; }",
                   statements[0].pretty_print());
    }

    #[test]
    fn function_with_one_argument() {
        let (tokens, _) = scan(&"fun add(x){return 2*x;}");
        let statements = parse(&tokens).unwrap();
        assert_eq!("fun add (x ) { return (* 2 x); }",
                   statements[0].pretty_print());
    }

    #[test]
    fn function_with_two_arguments() {
        let (tokens, _) = scan(&"fun add(x,y){return x + y;}");
        let statements = parse(&tokens).unwrap();
        assert_eq!("fun add (x y ) { return (+ x y); }",
                   statements[0].pretty_print());
    }
}
