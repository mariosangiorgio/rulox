use frontend::scanner::{scan_into_iterator, ScannerError, Token, TokenWithContext};
use std::iter::Peekable;
use vm::bytecode::Chunk;

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

enum RuleFunction {
    Grouping,
    Unary,
    Binary
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

    fn peek(&mut self) -> Option<&TokenWithContext> {
        match self.tokens.peek() {
            None => None,
            Some(Ok(t)) => Some(t),
            Some(Err(e)) => panic!("This should have been skipped while advancing"),
        }
    }

    fn advance(&mut self) -> Option<TokenWithContext> {
        loop {
            match self.tokens.next() {
                None => return None,
                Some(Ok(t)) => return Some(t),
                Some(Err(e)) => self.errors.push(CompilationError::ScannerError(e)),
            }
        }
    }

    fn find_rule(&mut self) -> Rule {
        match self.peek() {
            Some(t) => match t.token {
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
                _ => unimplemented!(),
                /*
  { NULL,     binary,  PREC_TERM },       // TOKEN_PLUS
  { NULL,     NULL,    PREC_NONE },       // TOKEN_SEMICOLON
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
  { number,   NULL,    PREC_NONE },       // TOKEN_NUMBER
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
      match rule_function{
        RuleFunction::Grouping => self.grouping(),
        RuleFunction::Unary => self.unary(),
        RuleFunction::Binary => self.binary(),
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

    fn grouping(&mut self) -> Result<(), ()> {
      unimplemented!()
    }

    fn unary(&mut self) -> Result<(), ()> {
      unimplemented!()
    }

    fn binary(&mut self) -> Result<(), ()> {
      unimplemented!()
    }
}

pub fn compile(text: &str) -> Chunk {
    let mut chunk = Chunk::new();
    let tokens = scan_into_iterator(text);
    {
        let mut parser = Parser::new(&mut chunk, tokens);
        parser.expression();
    }
    chunk
}
