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

//TODO: replace with "function pointers", if possible
enum RuleFunction {
    Grouping,
    Unary,
    Binary,
    Number,
}

struct Rule {
    precedence: Precedence,
    prefix: Option<RuleFunction>,
    infix: Option<RuleFunction>,
}

impl Rule {
    /// Finds the rule associated with a Token type.
    /// This is derived straight from the Pratt Parser table.
    fn find(token: &Token) -> Rule {
        match token {
            Token::LeftParen => Rule {
                precedence: Precedence::Call,
                prefix: Some(RuleFunction::Grouping),
                infix: None,
            },
            Token::RightParen => Rule {
                precedence: Precedence::None,
                prefix: None,
                infix: None,
            },
            Token::LeftBrace => Rule {
                precedence: Precedence::None,
                prefix: Some(RuleFunction::Grouping),
                infix: None,
            },
            Token::RightBrace => Rule {
                precedence: Precedence::None,
                prefix: None,
                infix: None,
            },
            Token::Comma => Rule {
                precedence: Precedence::None,
                prefix: None,
                infix: None,
            },
            Token::Dot => Rule {
                precedence: Precedence::Call,
                prefix: None,
                infix: None,
            },
            Token::Minus => Rule {
                precedence: Precedence::Term,
                prefix: Some(RuleFunction::Unary),
                infix: Some(RuleFunction::Binary),
            },
            Token::Plus => Rule {
                precedence: Precedence::Term,
                prefix: None,
                infix: Some(RuleFunction::Binary),
            },
            Token::Slash => Rule {
                precedence: Precedence::Factor,
                prefix: None,
                infix: Some(RuleFunction::Binary),
            },
            Token::Star => Rule {
                precedence: Precedence::Factor,
                prefix: None,
                infix: Some(RuleFunction::Binary),
            },
            Token::Semicolon => Rule {
                precedence: Precedence::None,
                prefix: None,
                infix: None,
            },
            Token::NumberLiteral(_) => Rule {
                precedence: Precedence::None,
                prefix: Some(RuleFunction::Number),
                infix: None,
            },
            _ => unimplemented!(),
            /*
  { NULL,     NULL,    PREC_NONE },       // TOKEN_BANG
  { NULL,     NULL,    PREC_EQUALITY },   // TOKEN_BANG_EQUAL
  { NULL,     NULL,    PREC_NONE },       // TOKEN_EQUAL
  { NULL,     NULL,    PREC_EQUALITY },   // TOKEN_EQUAL_EQUAL
  { NULL,     NULL,    PREC_COMPARISON }, // TOKEN_GREATER
  { NULL,     NULL,    PREC_COMPARISON }, // TOKEN_GREATER_EQUAL
  { NULL,     NULL,    PREC_COMPARISON }, // TOKEN_LESS
  { NULL,     NULL,    PREC_COMPARISON }, // TOKEN_LESS_EQUAL
  { NULL,     NULL,    PREC_NONE },       // TOKEN_IDENTIFIER
  { NULL,     NULL,    PREC_NONE },       // TOKEN_STRING
  { NULL,     NULL,    PREC_AND },        // TOKEN_AND
  { NULL,     NULL,    PREC_NONE },       // TOKEN_CLASS
  { NULL,     NULL,    PREC_NONE },       // TOKEN_ELSE
  { NULL,     NULL,    PREC_NONE },       // TOKEN_FALSE
  { NULL,     NULL,    PREC_NONE },       // TOKEN_FUN
  { NULL,     NULL,    PREC_NONE },       // TOKEN_FOR
  { NULL,     NULL,    PREC_NONE },       // TOKEN_IF
  { NULL,     NULL,    PREC_NONE },       // TOKEN_NIL
  { NULL,     NULL,    PREC_OR },         // TOKEN_OR
  { NULL,     NULL,    PREC_NONE },       // TOKEN_PRINT
  { NULL,     NULL,    PREC_NONE },       // TOKEN_RETURN
  { NULL,     NULL,    PREC_NONE },       // TOKEN_SUPER
  { NULL,     NULL,    PREC_NONE },       // TOKEN_THIS
  { NULL,     NULL,    PREC_NONE },       // TOKEN_TRUE
  { NULL,     NULL,    PREC_NONE },       // TOKEN_VAR
  { NULL,     NULL,    PREC_NONE },       // TOKEN_WHILE
  { NULL,     NULL,    PREC_NONE },       // TOKEN_ERROR
  { NULL,     NULL,    PREC_NONE },       // TOKEN_EOF
*/
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

    fn dispatch(&mut self, rule_function: RuleFunction) -> Result<(), ParsingError> {
        match rule_function {
            RuleFunction::Grouping => self.grouping(),
            RuleFunction::Unary => self.unary(),
            RuleFunction::Binary => self.binary(),
            RuleFunction::Number => self.number(),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), ParsingError> {
        let prefix_function = {
            let peeked = self.peek().ok_or(ParsingError::UnexpectedEndOfFile)?;
            Rule::find(&peeked.token)
                .prefix
                .ok_or_else(|| ParsingError::Unexpected(peeked.lexeme.clone(), peeked.position))?
        };
        self.dispatch(prefix_function)?;
        loop {
            let infix_function = {
                match self.peek() {
                    Some(peeked) => {
                        let infix_rule = Rule::find(&peeked.token);
                        if precedence <= infix_rule.precedence {
                            infix_rule.infix.ok_or_else(|| {
                                ParsingError::Unexpected(peeked.lexeme.clone(), peeked.position)
                            })?
                        } else {
                            return Ok(());
                        }
                    }
                    None => return Ok(()),
                }
            };
            self.dispatch(infix_function)?;
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
            Rule::find(&peeked.token).precedence.next()
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
