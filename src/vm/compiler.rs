use frontend::scanner::{scan_into_iterator, Position, ScannerError, Token, TokenWithContext};
use std::iter::Peekable;
use vm::bytecode::{BinaryOp, Chunk, OpCode};

enum ParsingError {
    UnexpectedEndOfFile,
    Unexpected(String, Position),
}

enum CompilationError {
    ScannerError(ScannerError),
    ParsingError(ParsingError),
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    None,
    Assignment,
    Or,
    And,
    Equality,
    Comparison,
    Term,
    Factor,
    Unary,
    Call,
    Primary,
}

impl Precedence {
    fn next(&self) -> Precedence {
        match *self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => panic!("What is it supposed to happen"),
        }
    }
}

struct Parser<'a, I>
where
    I: Iterator<Item = Result<TokenWithContext, ScannerError>>,
{
    chunk: &'a mut Chunk,
    tokens: Peekable<I>,
    errors: Vec<CompilationError>,
}

impl<'a, I> Parser<'a, I>
where
    I: Iterator<Item = Result<TokenWithContext, ScannerError>>,
{
    fn new(chunk: &'a mut Chunk, tokens: I) -> Parser<I> {
        Parser {
            chunk: chunk,
            tokens: tokens.peekable(),
            errors: vec![],
        }
    }

    fn skip_to_valid(&mut self) -> () {
        loop {
            match self.tokens.peek() {
                Some(Ok(TokenWithContext {
                    token: Token::Whitespace,
                    lexeme: _,
                    position: _,
                })) => {
                    // No op, just skip it
                }
                Some(Ok(TokenWithContext {
                    token: Token::Comment,
                    lexeme: _,
                    position: _,
                })) => {
                    // No op, just skip it
                }
                Some(Err(ref e)) => {
                    self.errors.push(CompilationError::ScannerError(e.clone()));
                }
                _ => return (),
            }
            let _ = self.tokens.next();
        }
    }

    /// Advances to the next valid token (skipping and keeping track of errors).
    /// This consumes an item from the token stream and returns it.
    fn advance(&mut self) -> Option<TokenWithContext> {
        // Peek to skip errors and keep track of them.
        // This makes unwrapping safe.
        self.skip_to_valid();
        self.tokens.next().map(|r| r.unwrap())
    }

    /// Adds an opcode to the current chunk
    fn emit(&mut self, opcode: OpCode, line: usize) -> () {
        self.chunk.add_instruction(opcode, line)
    }

    fn peek(&mut self) -> Option<&TokenWithContext> {
        self.skip_to_valid();
        self.tokens.peek().map(|result| match result {
            Ok(ref token_with_context) => token_with_context,
            Err(_) => unreachable!("We already skipped errors"),
        })
    }

    fn find_rule(
        token: &Token,
    ) -> (
        Precedence,
        Option<fn(&mut Self) -> Result<(), ParsingError>>,
        Option<fn(&mut Self) -> Result<(), ParsingError>>,
    )
    where
        I: Iterator<Item = Result<TokenWithContext, ScannerError>>,
    {
        // (Precedence, Prefix, Infix)
        match token {
            Token::LeftParen => (Precedence::Call, Some(Parser::grouping), None),
            Token::RightParen => (Precedence::None, None, None),
            Token::Comma => (Precedence::None, None, None),
            Token::Dot => (Precedence::Call, None, None),
            Token::Minus => (
                Precedence::Term,
                Some(Parser::unary),
                Some(Parser::binary),
            ),
            Token::Plus => (Precedence::Term, None, Some(Parser::binary)),
            Token::Slash => (Precedence::Factor, None, Some(Parser::binary)),
            Token::Star => (Precedence::Factor, None, Some(Parser::binary)),
            Token::Semicolon => (Precedence::None, None, None),
            Token::NumberLiteral(_) => (Precedence::None, Some(Parser::number), None),
            _ => unimplemented!(),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), ParsingError> {
        let prefix_function = {
            let peeked = self.peek().ok_or(ParsingError::UnexpectedEndOfFile)?;
            let (_, prefix_function, _) = Self::find_rule(&peeked.token);
            prefix_function
                .ok_or_else(|| ParsingError::Unexpected(peeked.lexeme.clone(), peeked.position))?
        };
        prefix_function(self)?;
        loop {
            let infix_function = {
                match self.peek() {
                    Some(peeked) => {
                        let (infix_rule_precedence, _, infix_function) =
                            Self::find_rule(&peeked.token);
                        if precedence <= infix_rule_precedence {
                            infix_function.ok_or_else(|| {
                                ParsingError::Unexpected(peeked.lexeme.clone(), peeked.position)
                            })?
                        } else {
                            return Ok(());
                        }
                    }
                    None => return Ok(()),
                }
            };
            infix_function(self)?;
        }
    }

    fn expression(&mut self) -> Result<(), ParsingError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn consume(&mut self, token: Token) -> Result<(), ParsingError> {
        let current = self.advance().ok_or(ParsingError::UnexpectedEndOfFile)?;
        if current.token == token {
            Ok(())
        } else {
            Err(ParsingError::Unexpected(
                current.lexeme.clone(),
                current.position,
            ))
        }
    }

    fn number(&mut self) -> Result<(), ParsingError> {
        let current = self.advance();
        let (value, line) = if let Some(ref t) = current {
            if let Token::NumberLiteral(ref n) = t.token {
                (*n, t.position.line)
            } else {
                unreachable!()
            }
        } else {
            panic!()
        };
        let constant = self.chunk.add_constant(value);
        self.chunk.add_instruction(OpCode::Constant(constant), line);
        Ok(())
    }

    fn grouping(&mut self) -> Result<(), ParsingError> {
        let _ = self.consume(Token::LeftParen)?;
        self.expression()?;
        self.consume(Token::RightParen)
    }

    fn unary(&mut self) -> Result<(), ParsingError> {
        let (opcode, line) = match self.advance() {
            Some(TokenWithContext {
                token: Token::Minus,
                position,
                lexeme: _,
            }) => (OpCode::Negate, position.line),
            _ => unreachable!("This code is executed only when we know we have a unary expression"),
        };
        let _ = self.parse_precedence(Precedence::Unary)?;
        self.emit(opcode, line);
        Ok(())
    }

    fn binary(&mut self) -> Result<(), ParsingError> {
        let (opcode, line) = match self.advance() {
            Some(TokenWithContext {
                token: Token::Plus,
                position,
                lexeme: _,
            }) => (OpCode::Binary(BinaryOp::Add), position.line),
            Some(TokenWithContext {
                token: Token::Minus,
                position,
                lexeme: _,
            }) => (OpCode::Binary(BinaryOp::Subtract), position.line),
            Some(TokenWithContext {
                token: Token::Star,
                position,
                lexeme: _,
            }) => (OpCode::Binary(BinaryOp::Multiply), position.line),
            Some(TokenWithContext {
                token: Token::Slash,
                position,
                lexeme: _,
            }) => (OpCode::Binary(BinaryOp::Divide), position.line),
            _ => {
                unreachable!("This code is executed only when we know we have a binary expression")
            }
        };
        let precedence = {
            let peeked = self
                .peek()
                .ok_or_else(|| ParsingError::UnexpectedEndOfFile)?;
            let (precedence, _, _) = Self::find_rule(&peeked.token);
            precedence.next()
        };
        self.parse_precedence(precedence)?;
        self.emit(opcode, line);
        Ok(())
    }

    fn parse(&mut self) -> Result<(), ()> {
        loop {
            match self.expression() {
                Ok(()) => return Ok(()),
                Err(error) => {
                    self.errors.push(CompilationError::ParsingError(error))
                    //TODO: add recovery mode
                }
            }
        }
    }
}

pub fn compile(text: &str) -> Result<Chunk, ()> {
    let mut chunk = Chunk::new();
    let tokens = scan_into_iterator(text);
    {
        let mut parser = Parser::new(&mut chunk, tokens);
        parser.parse()?;
        // TODO: assert that we consumed everything
        parser.emit(OpCode::Return, 0); // Line is meaningless
    }
    Ok(chunk)
}
