use frontend::scanner::{scan_into_iterator, ScannerError, Token, TokenWithContext};
use std::iter::Peekable;
use vm::bytecode::{BinaryOp, Chunk, OpCode};

enum CompilationError {
    ScannerError(ScannerError),
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
                precedence: Precedence::Call,
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

    /// Peeks for the next valid token.
    /// If necessary it will skip errors (and keep track of them)
    fn peek(&mut self) -> Option<Token> {
        loop {
            {
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
                    Some(Ok(ref t)) => return Some(t.token.clone()),
                    None => return None,
                    Some(Err(ref e)) => {
                        self.errors.push(CompilationError::ScannerError(e.clone()));
                    }
                }
            }
            {
                let _ = self.tokens.next();
            }
        }
    }

    /// Advances to the next valid token (skipping and keeping track of errors).
    /// This consumes an item from the token stream and returns it.
    fn advance(&mut self) -> Option<TokenWithContext> {
        // Peek to skip errors and keep track of them.
        // This makes unwrapping safe.
        self.peek();
        self.tokens.next().map(|r| r.unwrap())
    }

    /// Adds an opcode to the current chunk
    fn emit(&mut self, opcode: OpCode, line: usize) -> () {
        self.chunk.add_instruction(opcode, line)
    }

    fn dispatch(&mut self, rule_function: RuleFunction) -> Result<(), ()> {
        match rule_function {
            RuleFunction::Grouping => self.grouping(),
            RuleFunction::Unary => self.unary(),
            RuleFunction::Binary => self.binary(),
            RuleFunction::Number => self.number(),
        }
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<(), ()> {
        match self.peek() {
            Some(ref token) => {
                let prefix_rule = Rule::find(&token);
                match prefix_rule.prefix {
                    None => return Err(()), // TODO: make it meaningful. Was: error("Expect expression.");
                    Some(prefix_function) => {
                        try!{self.dispatch(prefix_function)}
                    }
                };
            }
            None => return Ok(()), // TODO: check if this is okay. Do we always expect something?
        }
        loop {
            match self.peek() {
                Some(ref token) => {
                    let infix_rule = Rule::find(&token);
                    if precedence <= infix_rule.precedence {
                        match infix_rule.infix {
                            None => return Err(()), // TODO: make it meaningful. Was: error("Expect expression.");
                            Some(prefix_function) => {
                                try!{self.dispatch(prefix_function)}
                            }
                        }
                    } else {
                        return Ok(());
                    }
                }
                None => return Ok(()), // TODO: check if this is okay. Do we always expect something?
            }
        }
    }

    fn expression(&mut self) -> Result<(), ()> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn consume(&mut self, token: Token) -> Result<(), ()> {
        let current = self.advance();
        if let Some(ref token_with_context) = current {
            if token_with_context.token == token {
                Ok(())
            } else {
                Err(())
            }
        } else {
            Err(()) //TODO: make it meaningful. Was "Expect ')' after expression.");
        }
    }

    fn number(&mut self) -> Result<(), ()> {
        let current = self.advance();
        let (value, line) = if let Some(ref t) = current {
            if let Token::NumberLiteral(ref n) = t.token {
                (*n, t.position.line)
            } else {
                // TODO: better message. To get here we had to peek a NumberLiteral
                panic!()
            }
        } else {
            panic!()
        };
        let constant = self.chunk.add_constant(value);
        self.chunk.add_instruction(OpCode::Constant(constant), line);
        Ok(())
    }

    fn grouping(&mut self) -> Result<(), ()> {
        self.expression()?;
        self.consume(Token::RightParen)
    }

    fn unary(&mut self) -> Result<(), ()> {
        let (opcode, line) = match self.advance() {
            Some(TokenWithContext {
                token: Token::Minus,
                position,
                lexeme: _,
            }) => (OpCode::Negate, position.line),
            _ => {
                // TODO: better error. To get here we had to peek
                panic!()
            }
        };
        let _ = self.expression()?;
        self.emit(opcode, line);
        Ok(())
    }

    fn binary(&mut self) -> Result<(), ()> {
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
                // TODO: better error. To get here we had to peek
                panic!()
            }
        };
        match self.peek() {
            Some(ref token) => {
                let rule = Rule::find(&token);
                self.parse_precedence(rule.precedence.next())?;
                self.emit(opcode, line);
                Ok(())
            }
            // TODO: I think this is not okay.
            // In any case, we need a good error message
            None => Err(()),
        }
    }
}

pub fn compile(text: &str) -> Result<Chunk, ()> {
    let mut chunk = Chunk::new();
    let tokens = scan_into_iterator(text);
    {
        let mut parser = Parser::new(&mut chunk, tokens);
        parser.expression()?;
        // TODO: assert that we consumed everything
        parser.emit(OpCode::Return, 0); // Line is meaningless
    }
    Ok(chunk)
}

#[cfg(test)]
mod tests {
    use vm::compiler::*;

    #[test]
    pub fn number() {
        let _ = compile("5").unwrap();
        // TODO: assert
    }

    #[test]
    pub fn unary() {
        let _ = compile("-5").unwrap();
        // TODO: assert
    }

    #[test]
    pub fn binary() {
        let _ = compile("5+10").unwrap();
        // TODO: assert
    }

    #[test]
    pub fn complex() {
        let _ = compile("-5+10*3").unwrap();
        // TODO: assert
    }
}
