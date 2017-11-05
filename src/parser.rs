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
    LeftBrace,
    RightBrace,
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

pub struct Parser {
    pub identifier_map: IdentifierMap,
    variable_use_handle_factory: VariableUseHandleFactory,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            identifier_map: IdentifierMap::new(),
            variable_use_handle_factory: VariableUseHandleFactory::new(),
        }
    }

    pub fn parse(&mut self,
                 tokens: &[TokenWithContext])
                 -> Result<Vec<Statement>, Vec<ParseError>> {
        let mut statements = Vec::new();
        let mut errors = Vec::new();
        let mut peekable_tokens = tokens.iter().peekable();
        while let Some(result) = self.parse_declaration(&mut peekable_tokens) {
            match result {
                Ok(statement) => {
                    statements.push(statement);
                }
                Err(error) => {
                    errors.push(error);
                    self.synchronise(&mut peekable_tokens);
                }
            }
        }
        if errors.is_empty() {
            Ok(statements)
        } else {
            Err(errors)
        }
    }

    fn consume_expected_identifier<'a, I>(&mut self,
                                          tokens: &mut Peekable<I>)
                                          -> Result<Identifier, ParseError>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        consume_expected_token_with_action!(tokens,
    &Token::Identifier(ref identifier),
    self.identifier_map.from_name(identifier),
    RequiredElement::Identifier)
    }

    fn synchronise<'a, I>(&mut self, tokens: &mut Peekable<I>)
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
                Token::Class | Token::Fun | Token::Var | Token::For | Token::If |
                Token::While | Token::Print | Token::Return => PositionInConstruct::Start,
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

    fn parse_semicolon_terminated_statement<'a, I>(&mut self,
                                            tokens: &mut Peekable<I>,
                                            parse_statement: &Fn(&mut Parser, &mut Peekable<I>)
                                                                 -> Option<Result<Statement,
                                                                                   ParseError>>)
                                            -> Option<Result<Statement, ParseError>>
where I: Iterator<Item = &'a TokenWithContext>{
        match parse_statement(self, tokens) {
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

    fn parse_declaration<'a, I>(&mut self,
                                tokens: &mut Peekable<I>)
                                -> Option<Result<Statement, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        match tokens.peek().map(|t| &t.token) {
            Some(&Token::Var) => {
                let _ = tokens.next();
                self.parse_semicolon_terminated_statement(tokens, &Parser::parse_var_declaration)
            }
            Some(&Token::LeftBrace) => {
                let _ = tokens.next();
                self.parse_block(tokens)
            }
            Some(&Token::Fun) => {
                let _ = tokens.next();
                self.parse_function_declaration(tokens, FunctionKind::Function)
            }
            Some(&Token::Class) => {
                let _ = tokens.next();
                self.parse_class_declaration(tokens)
            }
            Some(&Token::If) => {
                let _ = tokens.next();
                self.parse_if_statement(tokens)
            }
            Some(&Token::While) => {
                let _ = tokens.next();
                self.parse_while_statement(tokens)
            }
            Some(&Token::For) => {
                let _ = tokens.next();
                self.parse_for_statement(tokens)
            }
            Some(_) => self.parse_semicolon_terminated_statement(tokens, &Parser::parse_statement),
            None => None,
        }
    }

    fn parse_function_declaration<'a, I>(&mut self,
                                         tokens: &mut Peekable<I>,
                                         kind: FunctionKind)
                                         -> Option<Result<Statement, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        let identifier = try_wrap_err!(self.consume_expected_identifier(tokens));
        let parse_identifier = |parser: &mut Parser, tokens: &mut Peekable<I>| {
            Some(parser.consume_expected_identifier(tokens))
        };
        let arguments = try_wrap_err!(self.parse_function_arguments(tokens, &parse_identifier));
        let _ = try_wrap_err!(
                consume_expected_token!(tokens, &Token::LeftBrace, RequiredElement::Block));
        let block = match self.parse_block(tokens) {
            Some(Ok(block)) => block,
            Some(err) => return Some(err),
            None => return Some(Err(ParseError::UnexpectedEndOfFile)),
        };
        Some(Ok(Statement::FunctionDefinition(Rc::new(FunctionDefinition {
                                                          kind: kind,
                                                          name: identifier,
                                                          arguments: arguments,
                                                          body: block,
                                                      }))))
    }

    fn parse_class_declaration<'a, I>(&mut self,
                                      tokens: &mut Peekable<I>)
                                      -> Option<Result<Statement, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        let identifier = try_wrap_err!(self.consume_expected_identifier(tokens));
        let _ =
            try_wrap_err!(
                consume_expected_token!(tokens, &Token::LeftBrace, RequiredElement::LeftBrace));
        let mut methods = vec![];
        fn is_class_end(token: &&TokenWithContext) -> bool {
            match **token {
                TokenWithContext { token: Token::RightBrace, .. } => true,
                _ => false,
            }
        }
        while let Some(false) = tokens.peek().map(&is_class_end) {
            match self.parse_function_declaration(tokens, FunctionKind::Method) {
                Some(Ok(Statement::FunctionDefinition(method))) => methods.push(method),
                Some(Ok(_)) => panic!("Function parsing didn't return a function"),
                None => return Some(Err(ParseError::UnexpectedEndOfFile)),
                Some(Err(error)) => return Some(Err(error)),
            }

        }
        let _ =
            try_wrap_err!(
                consume_expected_token!(tokens, &Token::RightBrace, RequiredElement::RightBrace));
        Some(Ok(Statement::Class(Rc::new(ClassDefinition {
                                             name: identifier,
                                             methods: methods,
                                         }))))
    }

    fn parse_var_declaration<'a, I>(&mut self,
                                    tokens: &mut Peekable<I>)
                                    -> Option<Result<Statement, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        let identifier = try_wrap_err!(self.consume_expected_identifier(tokens));
        if let Some(&&TokenWithContext {
                        token: Token::Equal,
                        ref lexeme,
                        ref position,
                    }) = tokens.peek() {
            let _ = tokens.next();
            match self.parse_expression(tokens) {
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

    fn parse_block<'a, I>(&mut self,
                          tokens: &mut Peekable<I>)
                          -> Option<Result<Statement, ParseError>>
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
            match self.parse_declaration(tokens) {
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

    fn parse_statement<'a, I>(&mut self,
                              tokens: &mut Peekable<I>)
                              -> Option<Result<Statement, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        match tokens.peek().map(|t| &t.token) {
            Some(&Token::Print) => {
                let _ = tokens.next();
                self.parse_print_statement(tokens)
            }
            Some(&Token::Return) => {
                let _ = tokens.next();
                self.parse_return_statement(tokens)
            }
            Some(_) => self.parse_expression_statement(tokens),
            None => None,
        }
    }

    fn parse_print_statement<'a, I>(&mut self,
                                    tokens: &mut Peekable<I>)
                                    -> Option<Result<Statement, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        self.parse_expression(tokens)
            .map(|r| r.map(Statement::Print))
    }

    fn parse_return_statement<'a, I>(&mut self,
                                     tokens: &mut Peekable<I>)
                                     -> Option<Result<Statement, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        if let Some(&Token::Semicolon) = tokens.peek().map(|t| &t.token) {
            Some(Ok(Statement::Return(None)))
        } else {
            self.parse_expression(tokens)
                .map(|result| result.map(|expression| Statement::Return(Some(expression))))
        }
    }

    fn parse_if_statement<'a, I>(&mut self,
                                 tokens: &mut Peekable<I>)
                                 -> Option<Result<Statement, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        let _ = try_wrap_err!(
                consume_expected_token!(
                    tokens, &Token::LeftParen, RequiredElement::LeftParen));
        let condition = match self.parse_expression(tokens) {
            Some(Ok(expression)) => expression,
            Some(Err(error)) => return Some(Err(error)),
            None => return Some(Err(ParseError::UnexpectedEndOfFile)),
        };
        let _ = try_wrap_err!(
            consume_expected_token!(
                tokens, &Token::RightParen, RequiredElement::RightParen));
        // I'd rather use parse_block instead of parse_declaration
        // that would require the presence of the brackets
        let then_branch = match self.parse_declaration(tokens) {
            Some(Ok(statement)) => statement,
            Some(Err(error)) => return Some(Err(error)),
            None => return Some(Err(ParseError::UnexpectedEndOfFile)),
        };
        if let Some(&Token::Else) = tokens.peek().map(|t| &t.token) {
            let _ = tokens.next();
            let else_branch = match self.parse_declaration(tokens) {
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

    fn parse_while_statement<'a, I>(&mut self,
                                    tokens: &mut Peekable<I>)
                                    -> Option<Result<Statement, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        let _ = try_wrap_err!(
            consume_expected_token!(tokens, &Token::LeftParen, RequiredElement::LeftParen));
        let condition = match self.parse_expression(tokens) {
            Some(Ok(expression)) => expression,
            Some(Err(error)) => return Some(Err(error)),
            None => return Some(Err(ParseError::UnexpectedEndOfFile)),
        };
        let _ =
            try_wrap_err!(
            consume_expected_token!(tokens, &Token::RightParen, RequiredElement::RightParen));
        // I'd rather use parse_block instead of parse_declaration
        // that would require the presence of the brackets
        let body = match self.parse_declaration(tokens) {
            Some(Ok(statement)) => statement,
            Some(Err(error)) => return Some(Err(error)),
            None => return Some(Err(ParseError::UnexpectedEndOfFile)),
        };
        Some(Ok(Statement::While(Box::new(While {
                                              condition: condition,
                                              body: body,
                                          }))))
    }

    fn parse_for_statement<'a, I>(&mut self,
                                  tokens: &mut Peekable<I>)
                                  -> Option<Result<Statement, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        let _ = try_wrap_err!(
            consume_expected_token!(tokens, &Token::LeftParen, RequiredElement::LeftParen));
        let initializer = match tokens.peek().map(|t| &t.token) {
            Some(&Token::Semicolon) => None,
            Some(&Token::Var) => {
                let _ = tokens.next();
                match self.parse_var_declaration(tokens) {
                    Some(Ok(expression)) => Some(expression),
                    Some(Err(error)) => return Some(Err(error)),
                    None => return Some(Err(ParseError::UnexpectedEndOfFile)),
                }
            }
            _ => {
                match self.parse_expression_statement(tokens) {
                    Some(Ok(expression)) => Some(expression),
                    Some(Err(error)) => return Some(Err(error)),
                    None => return Some(Err(ParseError::UnexpectedEndOfFile)),
                }
            }
        };
        let _ = try_wrap_err!(
            consume_expected_token!(tokens, &Token::Semicolon, RequiredElement::Semicolon));

        let condition = match tokens.peek().map(|t| &t.token) {
            Some(&Token::Semicolon) => Expr::Literal(Literal::BoolLiteral(true)),
            _ => {
                match self.parse_expression(tokens) {
                    Some(Ok(expression)) => expression,
                    Some(Err(error)) => return Some(Err(error)),
                    None => return Some(Err(ParseError::UnexpectedEndOfFile)),
                }
            }
        };

        let _ = try_wrap_err!(
            consume_expected_token!(tokens, &Token::Semicolon, RequiredElement::Semicolon));

        let increment = match tokens.peek().map(|t| &t.token) {
            Some(&Token::RightParen) => None,
            _ => {
                match self.parse_expression(tokens) {
                    Some(Ok(expression)) => Some(expression),
                    Some(Err(error)) => return Some(Err(error)),
                    None => return Some(Err(ParseError::UnexpectedEndOfFile)),
                }
            }
        };
        let _ =
            try_wrap_err!(
            consume_expected_token!(tokens, &Token::RightParen, RequiredElement::RightParen));

        // I'd rather use parse_block instead of parse_declaration
        // that would require the presence of the brackets
        let body = match self.parse_declaration(tokens) {
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

    fn parse_expression_statement<'a, I>(&mut self,
                                         tokens: &mut Peekable<I>)
                                         -> Option<Result<Statement, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        self.parse_expression(tokens)
            .map(|r| r.map(Statement::Expression))
    }

    fn parse_expression<'a, I>(&mut self,
                               tokens: &mut Peekable<I>)
                               -> Option<Result<Expr, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        self.parse_assignment(tokens)
    }

    fn parse_logic<'a, I>(&mut self,
                          tokens: &mut Peekable<I>,
                          map_operator: &Fn(&Token) -> Option<LogicOperator>,
                          parse_subexpression: &Fn(&mut Parser, &mut Peekable<I>)
                                                   -> Option<Result<Expr, ParseError>>)
                          -> Option<Result<Expr, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        let mut expr;
        {
            let result = parse_subexpression(self, tokens);
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
            let right = if let Some(result) = parse_subexpression(self, tokens) {
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

    fn parse_binary<'a, I>(&mut self,
                           tokens: &mut Peekable<I>,
                           map_operator: &Fn(&Token) -> Option<BinaryOperator>,
                           parse_subexpression: &Fn(&mut Parser, &mut Peekable<I>)
                                                    -> Option<Result<Expr, ParseError>>)
                           -> Option<Result<Expr, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        let mut expr;
        {
            let result = parse_subexpression(self, tokens);
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
            let right = if let Some(result) = parse_subexpression(self, tokens) {
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

    fn parse_assignment<'a, I>(&mut self,
                               tokens: &mut Peekable<I>)
                               -> Option<Result<Expr, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        match self.parse_or(tokens) {
            Some(Ok(lvalue)) => {
                if let Some(&Token::Equal) = tokens.peek().map(|t| &t.token) {
                    let equal = tokens.next().unwrap();
                    match lvalue {
                        Expr::Identifier(_, identifier) => {
                            let target = Target::Identifier(identifier);
                            match self.parse_assignment(tokens) {
                                None => Some(Err(ParseError::UnexpectedEndOfFile)),
                                Some(result) => {
                                    Some(result.map(|rvalue| {
                                        Expr::Assignment(Box::new(Assignment {
                                            handle : self.variable_use_handle_factory.next(),
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

    fn parse_or<'a, I>(&mut self, tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        fn map_operator(token: &Token) -> Option<LogicOperator> {
            match *token {
                Token::Or => Some(LogicOperator::Or),
                _ => None,
            }
        }
        self.parse_logic(tokens, &map_operator, &Parser::parse_and)
    }

    fn parse_and<'a, I>(&mut self, tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        fn map_operator(token: &Token) -> Option<LogicOperator> {
            match *token {
                Token::And => Some(LogicOperator::And),
                _ => None,
            }
        }
        self.parse_logic(tokens, &map_operator, &Parser::parse_equality)
    }

    fn parse_equality<'a, I>(&mut self,
                             tokens: &mut Peekable<I>)
                             -> Option<Result<Expr, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        fn map_operator(token: &Token) -> Option<BinaryOperator> {
            match *token {
                Token::BangEqual => Some(BinaryOperator::NotEqual),
                Token::EqualEqual => Some(BinaryOperator::Equal),
                _ => None,
            }
        }
        self.parse_binary(tokens, &map_operator, &Parser::parse_comparison)
    }

    fn parse_comparison<'a, I>(&mut self,
                               tokens: &mut Peekable<I>)
                               -> Option<Result<Expr, ParseError>>
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
        self.parse_binary(tokens, &map_operator, &Parser::parse_term)
    }

    fn parse_term<'a, I>(&mut self, tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        fn map_operator(token: &Token) -> Option<BinaryOperator> {
            match *token {
                Token::Minus => Some(BinaryOperator::Minus),
                Token::Plus => Some(BinaryOperator::Plus),
                _ => None,
            }
        }
        self.parse_binary(tokens, &map_operator, &Parser::parse_factor)
    }

    fn parse_factor<'a, I>(&mut self, tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        fn map_operator(token: &Token) -> Option<BinaryOperator> {
            match *token {
                Token::Slash => Some(BinaryOperator::Slash),
                Token::Star => Some(BinaryOperator::Star),
                _ => None,
            }
        }
        self.parse_binary(tokens, &map_operator, &Parser::parse_unary)
    }

    fn parse_unary<'a, I>(&mut self, tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
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
            let right = if let Some(result) = self.parse_unary(tokens) {
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
            self.parse_call(tokens)
        }
    }

    fn parse_call<'a, I>(&mut self, tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        let mut expression = match self.parse_primary(tokens) {
            Some(Ok(expression)) => expression,
            error => return error,
        };
        while let Some(&Token::LeftParen) = tokens.peek().map(|t| &t.token) {
            expression = match self.finish_call(tokens, expression) {
                Some(Ok(expression)) => expression,
                error => return error,
            };
        }
        Some(Ok(expression))
    }

    fn parse_function_arguments<'a, I, A>(&mut self,
                                          tokens: &mut Peekable<I>,
                                          parse_argument: &Fn(&mut Parser, &mut Peekable<I>)
                                                              -> Option<Result<A, ParseError>>)
                                          -> Result<Vec<A>, ParseError>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        let _ = try!(
                consume_expected_token!(
                    tokens, &Token::LeftParen, RequiredElement::LeftParen));
        let mut arguments = vec![];
        if let Some(&Token::RightParen) = tokens.peek().map(|t| &t.token) {
        } else {
            loop {
                match parse_argument(self, tokens) {
                    Some(Ok(expression)) => {
                        if arguments.len() >= 8 {
                            return Err(ParseError::TooManyArguments);
                        }
                        arguments.push(expression)
                    }
                    Some(Err(error)) => return Err(error),
                    None => return Err(ParseError::UnexpectedEndOfFile),
                };
                if let Some(&Token::Comma) = tokens.peek().map(|t| &t.token) {
                    let _ = tokens.next();
                } else {
                    break;
                }
            }
        }
        let _ =
            try!(
            consume_expected_token!(tokens, &Token::RightParen, RequiredElement::RightParen));
        Ok(arguments)
    }

    fn finish_call<'a, I>(&mut self,
                          tokens: &mut Peekable<I>,
                          callee: Expr)
                          -> Option<Result<Expr, ParseError>>
        where I: Iterator<Item = &'a TokenWithContext>
    {
        let arguments =
            try_wrap_err!(self.parse_function_arguments(tokens, &Parser::parse_expression));
        Some(Ok(Expr::Call(Box::new(Call {
                                        callee: callee,
                                        arguments: arguments,
                                    }))))
    }

    fn parse_primary<'a, I>(&mut self, tokens: &mut Peekable<I>) -> Option<Result<Expr, ParseError>>
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
                Token::Identifier(ref i) => {
                    Expr::Identifier(self.variable_use_handle_factory.next(),
                                     self.identifier_map.from_name(i))
                }
                Token::LeftParen => {
                    let expr = if let Some(result) = self.parse_expression(tokens) {
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
        let mut parser = Parser::new();
        let expr = parser
            .parse_expression(&mut tokens.iter().peekable())
            .unwrap()
            .unwrap();
        assert_eq!(&string, &expr.pretty_print(&parser.identifier_map));
    }

    #[test]
    fn binary() {
        let (tokens, _) = scan(&"123+456");
        let mut parser = Parser::new();
        let expr = parser
            .parse_expression(&mut tokens.iter().peekable())
            .unwrap()
            .unwrap();
        assert_eq!("(+ 123 456)", &expr.pretty_print(&parser.identifier_map));
    }

    #[test]
    fn precedence_add_mul() {
        let (tokens, _) = scan(&"123+456*789");
        let mut parser = Parser::new();
        let expr = parser
            .parse_expression(&mut tokens.iter().peekable())
            .unwrap()
            .unwrap();
        assert_eq!("(+ 123 (* 456 789))", &expr.pretty_print(&parser.identifier_map));
    }

    #[test]
    fn precedence_mul_add() {
        let (tokens, _) = scan(&"123*456+789");
        let mut parser = Parser::new();
        let expr = parser
            .parse_expression(&mut tokens.iter().peekable())
            .unwrap()
            .unwrap();
        assert_eq!("(+ (* 123 456) 789)", &expr.pretty_print(&parser.identifier_map));
    }

    #[test]
    fn precedence_mul_add_unary() {
        let (tokens, _) = scan(&"-123*456+789");
        let mut parser = Parser::new();
        let expr = parser
            .parse_expression(&mut tokens.iter().peekable())
            .unwrap()
            .unwrap();
        assert_eq!("(+ (* (- 123) 456) 789)", &expr.pretty_print(&parser.identifier_map));
    }

    #[test]
    fn logic() {
        let (tokens, _) = scan(&"123 and 456");
        let mut parser = Parser::new();
        let expr = parser
            .parse_expression(&mut tokens.iter().peekable())
            .unwrap()
            .unwrap();
        assert_eq!("(and 123 456)", &expr.pretty_print(&parser.identifier_map));
    }

    #[test]
    fn precedence_or_and() {
        let (tokens, _) = scan(&"a or b and c");
        let mut parser = Parser::new();
        let expr = parser
            .parse_expression(&mut tokens.iter().peekable())
            .unwrap()
            .unwrap();
        assert_eq!("(or a (and b c))", &expr.pretty_print(&parser.identifier_map));
    }

    #[test]
    fn unclosed_group() {
        let (tokens, _) = scan(&"(2");
        assert!(Parser::new().parse(&tokens).is_err());
    }

    #[test]
    fn unopened_group() {
        let (tokens, _) = scan(&"2)");
        assert!(Parser::new().parse(&tokens).is_err());
    }

    #[test]
    fn multiple_statements() {
        let (tokens, _) = scan(&"var x; {var x=10; print x;} print x;");
        let mut parser = Parser::new();
        let statements = parser.parse(&tokens).unwrap();
        assert_eq!("var x;", statements[0].pretty_print(&parser.identifier_map));
        assert_eq!("{ var x = 10; print x; }", statements[1].pretty_print(&parser.identifier_map));
        assert_eq!("print x;", statements[2].pretty_print(&parser.identifier_map));
    }

    #[test]
    fn expr_with_equality() {
        let (tokens, _) = scan(&"x/2 == 1;");
        let mut parser = Parser::new();
        let statements = parser.parse(&tokens).unwrap();
        assert_eq!("(== (/ x 2) 1);", statements[0].pretty_print(&parser.identifier_map));
    }

    #[test]
    fn assignment() {
        let (tokens, _) = scan(&"a = 1;");
        let mut parser = Parser::new();
        let statements = parser.parse(&tokens).unwrap();
        assert_eq!("a = 1;", statements[0].pretty_print(&parser.identifier_map));
    }

    #[test]
    fn wrong_variable_declaration_target() {
        let (tokens, _) = scan(&"var 1 = 1;");
        assert!(Parser::new().parse(&tokens).is_err());
    }

    #[test]
    fn wrong_assigment_target() {
        let (tokens, _) = scan(&"1 = 1;");
        assert!(Parser::new().parse(&tokens).is_err());
    }

    #[test]
    fn missing_initializer() {
        let (tokens, _) = scan(&"var a =;");
        assert!(Parser::new().parse(&tokens).is_err());
    }

    #[test]
    fn unfinished_block() {
        let (tokens, _) = scan(&"{var a = 1;");
        assert!(Parser::new().parse(&tokens).is_err());
    }

    #[test]
    fn if_then_statement() {
        let (tokens, _) = scan(&"if(a) x = 2;");
        let mut parser = Parser::new();
        let statements = parser.parse(&tokens).unwrap();
        assert_eq!("if ( a ) x = 2;", statements[0].pretty_print(&parser.identifier_map));
    }

    #[test]
    fn if_then_else_statement() {
        let (tokens, _) = scan(&"if(a and b) { x = 2;} else{x = 3;}");
        let mut parser = Parser::new();
        let statements = parser.parse(&tokens).unwrap();
        assert_eq!("if ( (and a b) ) { x = 2; } else { x = 3; }",
                   statements[0].pretty_print(&parser.identifier_map));
    }

    #[test]
    fn while_statement() {
        let (tokens, _) = scan(&"while(a > 0){ a = a - 1;}");
        let mut parser = Parser::new();
        let statements = parser.parse(&tokens).unwrap();
        assert_eq!("while ( (> a 0) ) { a = (- a 1); }",
                   statements[0].pretty_print(&parser.identifier_map));
    }

    #[test]
    fn for_statement() {
        let (tokens, _) = scan(&"for (var i = 0; i < 10; i = i + 1) print i;");
        let mut parser = Parser::new();
        let statements = parser.parse(&tokens).unwrap();
        assert_eq!("{ var i = 0; while ( (< i 10) ) { print i; i = (+ i 1); } }",
                   statements[0].pretty_print(&parser.identifier_map));
    }
    #[test]
    fn for_statement_no_initializer() {
        let (tokens, _) = scan(&"for (; i < 10; i = i + 1) print i;");
        let mut parser = Parser::new();
        let statements = parser.parse(&tokens).unwrap();
        assert_eq!("while ( (< i 10) ) { print i; i = (+ i 1); }",
                   statements[0].pretty_print(&parser.identifier_map));
    }
    #[test]
    fn for_statement_no_condition() {
        let (tokens, _) = scan(&"for (var i = 0;; i = i + 1) print i;");
        let mut parser = Parser::new();
        let statements = parser.parse(&tokens).unwrap();
        assert_eq!("{ var i = 0; while ( true ) { print i; i = (+ i 1); } }",
                   statements[0].pretty_print(&parser.identifier_map));
    }

    #[test]
    fn for_statement_no_increment() {
        let (tokens, _) = scan(&"for (var i = 0; i < 10;) print i;");
        let mut parser = Parser::new();
        let statements = parser.parse(&tokens).unwrap();
        assert_eq!("{ var i = 0; while ( (< i 10) ) print i; }",
                   statements[0].pretty_print(&parser.identifier_map));
    }

    #[test]
    fn function_with_no_arguments() {
        let (tokens, _) = scan(&"fun add(){return 1;}");
        let mut parser = Parser::new();
        let statements = parser.parse(&tokens).unwrap();
        assert_eq!("fun add () { return 1; }",
                   statements[0].pretty_print(&parser.identifier_map));
    }

    #[test]
    fn function_with_one_argument() {
        let (tokens, _) = scan(&"fun add(x){return 2*x;}");
        let mut parser = Parser::new();
        let statements = parser.parse(&tokens).unwrap();
        assert_eq!("fun add (x ) { return (* 2 x); }",
                   statements[0].pretty_print(&parser.identifier_map));
    }

    #[test]
    fn function_with_two_arguments() {
        let (tokens, _) = scan(&"fun add(x,y){return x + y;}");
        let mut parser = Parser::new();
        let statements = parser.parse(&tokens).unwrap();
        assert_eq!("fun add (x y ) { return (+ x y); }",
                   statements[0].pretty_print(&parser.identifier_map));
    }

    #[test]
    fn class() {
        let (tokens, _) = scan(&"class A{m(){print 1;}}");
        let mut parser = Parser::new();
        let statements = parser.parse(&tokens).unwrap();
        assert_eq!("class A { m () { print 1; } }",
                   statements[0].pretty_print(&parser.identifier_map));
    }
}
