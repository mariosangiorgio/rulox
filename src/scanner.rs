use itertools::{multipeek, MultiPeek};
use std::str;

#[derive(Debug, PartialEq)]
pub enum Token {
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Identifier(String),
    StringLiteral(String),
    NumberLiteral(f64),
    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
    // The book doesn't have tokens for comments and
    // whitespaces. Introducing them the scanner can
    // deal with them uniformly and in a more
    // functional way.
    Comment,
    Whitespace,
}

#[derive(Debug)]
pub struct TokenWithContext {
    token: Token,
    // Takes a copy. Tokens can outlive the file they came from
    lexeme: String,
    line: usize,
}

struct Scanner<'a> {
    current: usize,
    current_lexeme: String,
    line: usize,
    source: MultiPeek<str::Chars<'a>>,
}

fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}

fn is_alpha(c: char) -> bool {
    (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
}

fn is_alphanumeric(c: char) -> bool {
    is_digit(c) || is_alpha(c)
}

fn is_whitespace(c: char) -> bool {
    match c {
        ' ' => true,
        '\r' => true,
        '\t' => true,
        '\n' => true,
        _ => false,
    }
}

impl<'a> Scanner<'a> {
    fn initialize(source: &String) -> Scanner {
        Scanner {
            current: 0,
            current_lexeme: "".into(),
            line: 1, // 1-indexed,
            source: multipeek(source.chars()),
        }
    }

    fn is_at_end(&mut self) -> bool {
        self.source.reset_peek();
        match self.source.peek() {
            Some(_) => false,
            None => true,
        }
    }

    fn peek_check(&mut self, check: &Fn(char) -> bool) -> bool {
        self.source.reset_peek();
        match self.source.peek() {
            Some(&c) => check(c),
            None => false,
        }
    }

    fn peek_check2(&mut self, check1: &Fn(char) -> bool, check2: &Fn(char) -> bool) -> bool {
        self.source.reset_peek();
        match self.source.peek() {
            Some(&p1) => {
                match self.source.peek() {
                    Some(&p2) => check1(p1) && check2(p2),
                    None => false,
                }
            }
            None => false,
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        // TODO: handle None!
        let c = self.source.next().unwrap();
        self.current_lexeme.push(c);
        if c == '\n' {
            self.line += 1;
        }
        c
    }

    fn is_match(&mut self, expected: char) -> bool {
        self.source.reset_peek();
        match self.source.peek() {
            Some(&c) => {
                if c == expected {
                    let _ = self.source.next();
                    self.current += 1;
                    true
                } else {
                    false
                }
            }
            None => false,
        }
    }

    fn add_context(&mut self, token: Token) -> TokenWithContext {
        let result = TokenWithContext {
            token: token,
            lexeme: self.current_lexeme.clone(),
            line: self.line,
        };
        self.current_lexeme = "".into();
        result
    }

    fn string(&mut self) -> Result<Token, String> {
        let initial_line = self.line;
        self.source.reset_peek();
        while self.peek_check(&|c| c != '"') {
            self.advance();
        }
        if self.is_at_end() {
            return Err(format!("Unterminated string at line {}", initial_line));
        }
        self.advance();
        let literal_length = self.current_lexeme.len() - 2;
        // Trims delimiters
        let literal = self.current_lexeme.chars().skip(1).take(literal_length).collect();
        Ok(Token::StringLiteral(literal))
    }

    fn number(&mut self) -> Token {
        while self.peek_check(&is_digit) {
            self.advance();
        }
        if self.peek_check2(&|p1| p1 == '.', &is_digit) {
            self.advance(); // Consume the .
            while self.peek_check(&is_digit) {
                self.advance();
            }
        }
        let value = self.current_lexeme.parse::<f64>().unwrap();
        Token::NumberLiteral(value)
    }

    fn identifier(&mut self) -> Token {
        while self.peek_check(&is_alphanumeric) {
            self.advance();
        }
        // TODO: take a ref in the first place
        match self.current_lexeme.as_ref() {
            "and" => Token::And,
            "class" => Token::Class,
            "else" => Token::Else,
            "false" => Token::False,
            "for" => Token::For,
            "fun" => Token::Fun,
            "if" => Token::If,
            "nil" => Token::Nil,
            "or" => Token::Or,
            "print" => Token::Print,
            "return" => Token::Return,
            "super" => Token::Super,
            "this" => Token::This,
            "true" => Token::True,
            "var" => Token::Var,
            "while" => Token::While,
            identifier => Token::Identifier(identifier.into()),
        }

    }

    fn scan_next(&mut self) -> Result<TokenWithContext, String> {
        let token = match self.advance() {
            '(' => Token::LeftParen,
            ')' => Token::RightParen,
            '{' => Token::LeftBrace,
            '}' => Token::RightBrace,
            ',' => Token::Comma,
            '.' => Token::Dot,
            '-' => Token::Minus,
            '+' => Token::Plus,
            ';' => Token::Semicolon,
            '*' => Token::Star,
            '!' => {
                if self.is_match('=') {
                    Token::BangEqual
                } else {
                    Token::Bang
                }
            }
            '=' => {
                if self.is_match('=') {
                    Token::EqualEqual
                } else {
                    Token::Equal
                }
            }
            '<' => {
                if self.is_match('=') {
                    Token::LessEqual
                } else {
                    Token::Less
                }
            }
            '>' => {
                if self.is_match('=') {
                    Token::GreaterEqual
                } else {
                    Token::Greater
                }
            }
            '/' => {
                if self.is_match('/') {
                    // Comments go on till the end of the line
                    while self.peek_check(&|c| c != '\n') {
                        self.advance();
                    }
                    Token::Comment
                } else {
                    Token::Slash
                }
            }
            '"' => try!(self.string()),
            c if is_whitespace(c) => Token::Whitespace,
            c if is_digit(c) => self.number(),
            c if is_alpha(c) => self.identifier(),
            c => {
                return Err(format!("Unexpected character {} at line {}, pos {}",
                                   c,
                                   self.line,
                                   self.current - 1));
            }
        };
        Ok(self.add_context(token))
    }
}


pub fn scan(source: &String) -> Result<Vec<TokenWithContext>, String> {
    let mut scanner = Scanner::initialize(source);
    let mut tokens = Vec::new();
    while !scanner.is_at_end() {
        let token_with_context = try!(scanner.scan_next());
        match token_with_context.token {
            // Ignoring tokens we don't care about
            Token::Comment => {}
            Token::Whitespace => {}
            _ => tokens.push(token_with_context),
        }
    }
    tokens.push(TokenWithContext {
        token: Token::Eof,
        lexeme: "".into(),
        line: scanner.line,
    });
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use scanner::*;

    #[test]
    fn single_token() {
        let tokens = scan(&"+".into()).unwrap();
        assert_eq!(tokens[0].token, Token::Plus);
    }

    #[test]
    fn expression() {
        let tokens = scan(&"1+2".into()).unwrap();
        assert_eq!(tokens[0].token, Token::NumberLiteral(1.0f64));
        assert_eq!(tokens[1].token, Token::Plus);
        assert_eq!(tokens[2].token, Token::NumberLiteral(2.0f64));
        assert_eq!(tokens[3].token, Token::Eof);
    }

    #[test]
    fn expression_with_whitespaces() {
        let tokens = scan(&"1 + 2".into()).unwrap();
        assert_eq!(tokens[0].token, Token::NumberLiteral(1.0f64));
        assert_eq!(tokens[1].token, Token::Plus);
        assert_eq!(tokens[2].token, Token::NumberLiteral(2.0f64));
        assert_eq!(tokens[3].token, Token::Eof);
    }

    #[test]
    fn assignement_with_comment() {
        let tokens = scan(&"var a = 1.0; // A comment".into()).unwrap();
        assert_eq!(tokens[0].token, Token::Var);
        assert_eq!(tokens[1].token, Token::Identifier("a".into()));
        assert_eq!(tokens[2].token, Token::Equal);
        assert_eq!(tokens[3].token, Token::NumberLiteral(1.0f64));
        assert_eq!(tokens[4].token, Token::Semicolon);
        assert_eq!(tokens[5].token, Token::Eof);
    }

    #[test]
    fn multiline_statements() {
        let tokens = scan(&r#"var a = 1.0;
                              var b = "Hello";"#
                .into())
            .unwrap();
        assert_eq!(tokens[0].token, Token::Var);
        assert_eq!(tokens[1].token, Token::Identifier("a".into()));
        assert_eq!(tokens[2].token, Token::Equal);
        assert_eq!(tokens[3].token, Token::NumberLiteral(1.0f64));
        assert_eq!(tokens[4].token, Token::Semicolon);
        assert_eq!(tokens[5].token, Token::Var);
        assert_eq!(tokens[6].token, Token::Identifier("b".into()));
        assert_eq!(tokens[7].token, Token::Equal);
        assert_eq!(tokens[8].token, Token::StringLiteral("Hello".into()));
        assert_eq!(tokens[9].token, Token::Semicolon);
        assert_eq!(tokens[10].token, Token::Eof);
        assert_eq!(tokens[1].line, 1);
        assert_eq!(tokens[9].line, 2);
    }
}
