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

#[derive(Debug,Clone,Copy)]
/// Represents a position in the source file.
/// Both line and column are represented by a 1-based
/// index, since this struct exists only to provide
/// context to the user.
struct Position {
    line: usize,
    column: usize,
}

impl Position {
    fn initial() -> Position {
        Position {
            line: 1,
            column: 1,
        }
    }

    fn increment_column(&mut self) -> () {
        self.column += 1;
    }

    fn increment_line(&mut self) -> () {
        self.line += 1;
        self.column = 1;
    }
}

#[derive(Debug)]
pub struct TokenWithContext {
    token: Token,
    // Takes a copy. Tokens can outlive the file they came from
    lexeme: String,
    /// Position of the first character of the token
    position: Position,
}

struct Scanner<'a> {
    current_position: Position,
    current_lexeme: String,
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
            current_position: Position::initial(),
            current_lexeme: "".into(),
            source: multipeek(source.chars()),
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

    fn advance(&mut self) -> Option<char> {
        let next = self.source.next();
        match next {
            Some(c) => {
                self.current_lexeme.push(c);
                if c == '\n' {
                    self.current_position.increment_line();
                } else {
                    self.current_position.increment_column();;
                }
            }
            None => (),
        }
        next
    }

    fn advance_if_match(&mut self, expected: char) -> bool {
        if self.peek_check(&|c| c == expected) {
            let _ = self.advance();
            true
        } else {
            false
        }
    }

    fn add_context(&mut self, token: Token, initial_position: Position) -> TokenWithContext {
        let result = TokenWithContext {
            token: token,
            lexeme: self.current_lexeme.clone(),
            position: initial_position,
        };
        self.current_lexeme = "".into();
        result
    }

    fn string(&mut self) -> Result<Token, String> {
        let initial_line = self.current_position.line;
        self.source.reset_peek();
        while self.peek_check(&|c| c != '"') {
            self.advance();
        }
        if !self.advance_if_match('"') {
            return Err(format!("Unterminated string at line {}", initial_line));
        }
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
        let initial_position = self.current_position;
        // Check if there is something left to iterate on.
        // Early return if we're done.
        let next_char = match self.advance() {
            Some(c) => c,
            None => {
                return Ok(TokenWithContext {
                    token: Token::Eof,
                    // Not very meaningful. Is Eof needed at all?
                    lexeme: "".into(),
                    position: initial_position,
                });
            }
        };
        let token = match next_char {
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
                if self.advance_if_match('=') {
                    Token::BangEqual
                } else {
                    Token::Bang
                }
            }
            '=' => {
                if self.advance_if_match('=') {
                    Token::EqualEqual
                } else {
                    Token::Equal
                }
            }
            '<' => {
                if self.advance_if_match('=') {
                    Token::LessEqual
                } else {
                    Token::Less
                }
            }
            '>' => {
                if self.advance_if_match('=') {
                    Token::GreaterEqual
                } else {
                    Token::Greater
                }
            }
            '/' => {
                if self.advance_if_match('/') {
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
                return Err(format!("Unexpected character {} at line {}, column {}",
                                   c,
                                   self.current_position.line,
                                   self.current_position.column - 1));
            }
        };
        Ok(self.add_context(token, initial_position))
    }
}


pub fn scan(source: &String) -> Result<Vec<TokenWithContext>, String> {
    let mut scanner = Scanner::initialize(source);
    let mut tokens = Vec::new();
    let mut eof = false;
    while !eof {
        let token_with_context = try!(scanner.scan_next());
        match token_with_context.token {
            // Ignoring tokens we don't care about
            Token::Comment => {}
            Token::Whitespace => {}
            Token::Eof => {
                eof = true;
                tokens.push(token_with_context);
            }
            _ => tokens.push(token_with_context),
        }
    }
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
        assert_eq!(tokens[1].position.line, 1);
        assert_eq!(tokens[1].position.column, 5);
        assert_eq!(tokens[9].position.line, 2);
        assert_eq!(tokens[9].position.column, 46); // There are 30 spaces before the text!
    }
}
