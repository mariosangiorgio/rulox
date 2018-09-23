use frontend::scanner::{scan_into_iterator, Position, ScannerError, Token, TokenWithContext};
use num_traits::{FromPrimitive, ToPrimitive};
use std::iter::Peekable;
use vm::bytecode::{BinaryOp, Chunk, OpCode};

#[derive(Debug)]
pub enum ParsingError {
    /// The end of file has been reached but the parser expected
    /// to find some other token
    UnexpectedEndOfFile,
    /// The parser was expecting a specific token (TODO: we should
    /// report which) but it instead found the reported lexeme
    Unexpected(String, Position),
}

#[derive(Debug)]
pub enum CompilationError {
    ScannerError(ScannerError),
    ParsingError(ParsingError),
}

#[derive(PartialEq, PartialOrd, FromPrimitive, ToPrimitive)]
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
    /// Returns the next (as in the immediately higher) Precedence.
    /// Note that this is not defined for the item with the highest
    /// precedence. In such case you'll get a panic
    fn next(&self) -> Precedence {
        // This reduces some boilerplate.
        Precedence::from_u8(self.to_u8().unwrap() + 1).unwrap()
    }
}

/// A single-pass Pratt Parser that consumes tokens from an iterator,
/// parses them into a Lox programs and emits a chunk of bytecode.
/// The parser also keeps tracks of errors.
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

    /// Ignores irrelevant (comments and whitespaces) and invalid
    /// tokens.
    /// When invalid tokens are encountered a corresponding error
    /// is generated so that they can be reported and compilation
    /// fails.
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

    /// Peeks the first *valid* token in the iterator
    fn peek(&mut self) -> Option<&TokenWithContext> {
        self.skip_to_valid();
        self.tokens.peek().map(|result| match result {
            Ok(ref token_with_context) => token_with_context,
            Err(_) => unreachable!("We already skipped errors"),
        })
    }

    /// Ensures that a specific token is the next in the input iterator.
    /// If that's the case, it will just consumes it.
    /// If not, it will return a parsing error.
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

    /// Represents the table for the Pratt Parser.
    /// Given the next token it will return a triple containing
    /// (Precedence, Prefix function, Infix function).
    /// The two functions are optional as they might not be specified.
    ///
    /// Note that this function doesn't care about the values possibly
    /// associated with a token. It operates only according to their
    /// variant.
    ///
    /// This is generally used on the token peeked from the front of
    /// the iterator. This lookahead allows us to decide what to do next
    /// according to the "table" below.
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
        match token {
            Token::LeftParen => (Precedence::Call, Some(Parser::grouping), None),
            Token::RightParen => (Precedence::None, None, None),
            Token::Comma => (Precedence::None, None, None),
            Token::Dot => (Precedence::Call, None, None),
            Token::Minus => (Precedence::Term, Some(Parser::unary), Some(Parser::binary)),
            Token::Plus => (Precedence::Term, None, Some(Parser::binary)),
            Token::Slash => (Precedence::Factor, None, Some(Parser::binary)),
            Token::Star => (Precedence::Factor, None, Some(Parser::binary)),
            Token::Semicolon => (Precedence::None, None, None),
            Token::NumberLiteral(_) => (Precedence::None, Some(Parser::number), None),
            _ => unimplemented!(),
        }
    }

    /// The core of a Pratt Parser.
    /// It peeks tokens to figure out what rule to apply and dispatches the corresponding
    /// functions.
    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), ParsingError> {
        let prefix_function = {
            let (_, prefix_function, _) = {
                let peeked = self.peek().ok_or(ParsingError::UnexpectedEndOfFile)?;
                Self::find_rule(&peeked.token)
            };
            prefix_function.ok_or_else(|| {
                // This is a bad token. We need to consume it so we can carry on.
                let token = self.advance().unwrap();
                ParsingError::Unexpected(token.lexeme.clone(), token.position)
            })?
        };
        prefix_function(self)?;
        loop {
            let infix_function = {
                let (infix_rule_precedence, _, infix_function) = {
                    match self.peek() {
                        Some(peeked) => Self::find_rule(&peeked.token),
                        None => return Ok(()),
                    }
                };
                if precedence <= infix_rule_precedence {
                    infix_function.ok_or_else(|| {
                        // This is a bad token. We need to consume it so we can carry on.
                        let token = self.advance().unwrap();
                        ParsingError::Unexpected(token.lexeme.clone(), token.position)
                    })?
                } else {
                    return Ok(());
                }
            };
            infix_function(self)?;
        }
    }

    /// Top level function of the parser.
    /// Parses all the statements in the input and, when necessary,
    /// applies the recovery logic for cleaner error messages.
    fn parse(&mut self) -> Result<(), ()> {
        while let Some(_) = self.peek() {
            if let Err(error) = self.expression() {
                self.errors.push(CompilationError::ParsingError(error));
            }
        }
        if self.errors.len() > 0 {
            Err(())
        } else {
            Ok(())
        }
    }

    fn expression(&mut self) -> Result<(), ParsingError> {
        self.parse_precedence(Precedence::Assignment)
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
}

/// Compiles a text producing either the corresponding chunk of bytecode
/// or an error.
/// Error reporting tries to be smart and to minimize reports adopting a
/// "recovery logic".
pub fn compile(text: &str) -> Result<Chunk, Vec<CompilationError>> {
    let mut chunk = Chunk::new();
    let tokens = scan_into_iterator(text);
    {
        let mut parser = Parser::new(&mut chunk, tokens);
        // Chunk is known, errors will be captured later
        // TODO: check if this is the best design
        let _ = parser.parse();
        // TODO: assert that we consumed everything
        // Line is meaningless, but this is temporary to see some results
        // while the implementation is in progress.
        parser.emit(OpCode::Return, 0);
        if parser.errors.len() > 0 {
            return Err(parser.errors);
        }
    }
    Ok(chunk)
}
