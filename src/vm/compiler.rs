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

struct Parser<'a, I>
where
    I: Iterator<Item = Result<TokenWithContext, ScannerError>>,
{
    chunk: &'a mut Chunk,
    previous: Option<TokenWithContext>,
    current: Option<TokenWithContext>,
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
            previous: None,
            current: None,
            tokens: tokens.peekable(),
            errors: vec![],
        }
    }

    fn shift(&mut self, current: Option<TokenWithContext>) -> () {
        self.previous = self.current.clone();
        self.current = current;
    }

    fn advance(&mut self) -> () {
        loop {
            match self.tokens.next() {
                None => {
                    self.shift(None);
                    return;
                }
                Some(Ok(t)) => {
                    self.shift(Some(t));
                    return;
                }
                Some(Err(e)) => self.errors.push(CompilationError::ScannerError(e)),
            }
        }
    }

    fn emit(&mut self, opcode: OpCode) -> () {
        if let Some(ref t) = self.previous {
            self.chunk.add_instruction(opcode, t.position.line)
        } else {
            // TODO: this is a bit lame
            panic!("How did we get here?");
        }
    }

    fn find_rule(&mut self) -> Rule {
        match self.current {
            Some(ref t) => match t.token {
                Token::LeftParen => Rule {
                    precedence: Precedence::Call,
                    prefix: Some(RuleFunction::Grouping),
                    infix: None,
                },
                Token::LeftParen => Rule {
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
                    precedence: Precedence::Call,
                    prefix: Some(RuleFunction::Unary),
                    infix: Some(RuleFunction::Binary),
                },
                Token::Plus => Rule {
                    precedence: Precedence::Term,
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
  { NULL,     binary,  PREC_FACTOR },     // TOKEN_SLASH
  { NULL,     binary,  PREC_FACTOR },     // TOKEN_STAR
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
            },
            None => unimplemented!(),
        }
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
        let prefix_rule = self.find_rule();
        match prefix_rule.prefix {
            None => return Err(()), // TODO: make it meaningful. Was: error("Expect expression.");
            Some(prefix_function) => {
                try!{self.dispatch(prefix_function)}
            }
        };
        loop {
            let infix_rule = self.find_rule();
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
    }

    fn expression(&mut self) -> Result<(), ()> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn consume(&mut self, token: Token) -> Result<(), ()> {
        self.advance();
        if let Some(ref token_with_context) = self.current {
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
        let (value, line) = if let Some(ref t) = self.previous {
            if let Token::NumberLiteral(ref n) = t.token {
                (*n, t.position.line)
            } else {
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
        let token = if let Some(ref t) = self.previous {
            t.token.clone()
        } else {
            panic!()
        };
        self.expression()?;
        match token {
            Token::Minus => {
                self.emit(OpCode::Negate);
                Ok(())
            }
            _ => unimplemented!(),
        }
    }

    fn binary(&mut self) -> Result<(), ()> {
        let token = if let Some(ref t) = self.previous {
            t.token.clone()
        } else {
            panic!()
        };
        let rule = self.find_rule();
        self.parse_precedence(rule.precedence.next())?;
        match token {
            Token::Plus => {
                self.emit(OpCode::Binary(BinaryOp::Add));
                Ok(())
            }
            Token::Minus => {
                self.emit(OpCode::Binary(BinaryOp::Subtract));
                Ok(())
            }
            Token::Star => {
                self.emit(OpCode::Binary(BinaryOp::Multiply));
                Ok(())
            }
            Token::Slash => {
                self.emit(OpCode::Binary(BinaryOp::Divide));
                Ok(())
            }
            _ => unimplemented!(),
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
        parser.emit(OpCode::Return);
    }
    Ok(chunk)
}
