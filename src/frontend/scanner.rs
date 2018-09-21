use itertools::{multipeek, MultiPeek};
use std::str;

#[derive(Debug, PartialEq, Clone)]
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

    // The book doesn't have tokens for comments and
    // whitespaces. Introducing them the scanner can
    // deal with them uniformly and in a more
    // functional way.
    Comment,
    Whitespace,
}

#[derive(Debug, Clone, Copy)]
/// Represents a position in the source file.
/// Both line and column are represented by a 1-based
/// index, since this struct exists only to provide
/// context to the user.
pub struct Position {
    pub line: usize,
    column: usize,
}

impl Position {
    fn initial() -> Position {
        Position { line: 1, column: 1 }
    }

    fn increment_column(&mut self) -> () {
        self.column += 1;
    }

    fn increment_line(&mut self) -> () {
        self.line += 1;
        self.column = 1;
    }
}

pub type Lexeme = String;

#[derive(Debug)]
pub struct TokenWithContext {
    pub token: Token,
    // Takes a copy. Tokens can outlive the file they came from
    pub lexeme: Lexeme,
    /// Position of the first character of the token
    pub position: Position,
}

#[derive(Debug, Clone)]
pub enum ScannerError {
    MissingStringTerminator(Position),
    UnexpectedCharacter(char, Position),
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
        ' ' | '\r' | '\t' | '\n' => true,
        _ => false,
    }
}

impl<'a> Scanner<'a> {
    fn initialize(source: &'a str) -> Scanner {
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
            Some(&p1) => match self.source.peek() {
                Some(&p2) => check1(p1) && check2(p2),
                None => false,
            },
            None => false,
        }
    }

    fn advance(&mut self) -> Option<char> {
        let next = self.source.next();
        if let Some(c) = next {
            self.current_lexeme.push(c);
            if c == '\n' {
                self.current_position.increment_line();
            } else {
                self.current_position.increment_column();;
            }
        };
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

    fn advance_while(&mut self, condition: &Fn(char) -> bool) -> () {
        while self.peek_check(condition) {
            self.advance();
        }
    }

    fn add_context(&mut self, token: Token, initial_position: Position) -> TokenWithContext {
        TokenWithContext {
            token: token,
            lexeme: self.current_lexeme.clone(),
            position: initial_position,
        }
    }

    fn string(&mut self) -> Result<Token, ScannerError> {
        self.advance_while(&|c| c != '"' && c != '\n');
        if !self.advance_if_match('"') {
            return Err(ScannerError::MissingStringTerminator(self.current_position));
        }
        let literal_length = self.current_lexeme.len() - 2;
        // Trims delimiters
        let literal = self
            .current_lexeme
            .chars()
            .skip(1)
            .take(literal_length)
            .collect();
        Ok(Token::StringLiteral(literal))
    }

    fn number(&mut self) -> Token {
        self.advance_while(&is_digit);
        if self.peek_check2(&|p1| p1 == '.', &is_digit) {
            self.advance(); // Consume the .
            self.advance_while(&is_digit);
        }
        let value = self.current_lexeme.parse::<f64>().unwrap();
        Token::NumberLiteral(value)
    }

    fn identifier(&mut self) -> Token {
        self.advance_while(&is_alphanumeric);
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

    fn scan_next(&mut self) -> Option<Result<TokenWithContext, ScannerError>> {
        let initial_position = self.current_position;
        self.current_lexeme.clear();
        // Check if there is something left to iterate on.
        // Early return if we're done.
        let next_char = match self.advance() {
            Some(c) => c,
            None => return None,
        };
        let result = match next_char {
            '(' => Ok(Token::LeftParen),
            ')' => Ok(Token::RightParen),
            '{' => Ok(Token::LeftBrace),
            '}' => Ok(Token::RightBrace),
            ',' => Ok(Token::Comma),
            '.' => Ok(Token::Dot),
            '-' => Ok(Token::Minus),
            '+' => Ok(Token::Plus),
            ';' => Ok(Token::Semicolon),
            '*' => Ok(Token::Star),
            '!' => {
                if self.advance_if_match('=') {
                    Ok(Token::BangEqual)
                } else {
                    Ok(Token::Bang)
                }
            }
            '=' => {
                if self.advance_if_match('=') {
                    Ok(Token::EqualEqual)
                } else {
                    Ok(Token::Equal)
                }
            }
            '<' => {
                if self.advance_if_match('=') {
                    Ok(Token::LessEqual)
                } else {
                    Ok(Token::Less)
                }
            }
            '>' => {
                if self.advance_if_match('=') {
                    Ok(Token::GreaterEqual)
                } else {
                    Ok(Token::Greater)
                }
            }
            '/' => {
                if self.advance_if_match('/') {
                    // Comments go on till the end of the line
                    self.advance_while(&|c| c != '\n');
                    Ok(Token::Comment)
                } else {
                    Ok(Token::Slash)
                }
            }
            '"' => self.string(),
            c if is_whitespace(c) => Ok(Token::Whitespace),
            c if is_digit(c) => Ok(self.number()),
            c if is_alpha(c) => Ok(self.identifier()),
            c => Err(ScannerError::UnexpectedCharacter(c, self.current_position)),
        };
        Some(result.map(|token| self.add_context(token, initial_position)))
    }
}

struct TokensIterator<'a> {
    scanner: Scanner<'a>,
}

impl<'a> Iterator for TokensIterator<'a> {
    type Item = Result<TokenWithContext, ScannerError>;
    fn next(&mut self) -> Option<Self::Item> {
        self.scanner.scan_next()
    }
}

pub fn scan_into_iterator<'a>(
    source: &'a str,
) -> impl Iterator<Item = Result<TokenWithContext, ScannerError>> + 'a {
    TokensIterator {
        scanner: Scanner::initialize(source),
    }
}

pub fn scan(source: &str) -> (Vec<TokenWithContext>, Vec<ScannerError>) {
    let mut tokens = Vec::new();
    let mut errors = Vec::new();
    for result in scan_into_iterator(source) {
        match result {
            Ok(token_with_context) => {
                match token_with_context.token {
                    // Ignoring tokens we don't care about
                    Token::Comment | Token::Whitespace => {}
                    _ => tokens.push(token_with_context),
                };
            }
            Err(error) => errors.push(error),
        }
    }
    (tokens, errors)
}

#[cfg(test)]
mod tests {
    use frontend::scanner::*;

    #[test]
    fn single_token() {
        let (tokens, _) = scan(&"+");
        assert_eq!(tokens[0].token, Token::Plus);
    }

    #[test]
    fn expression() {
        let (tokens, _) = scan(&"1+2");
        assert_eq!(tokens[0].token, Token::NumberLiteral(1.0f64));
        assert_eq!(tokens[1].token, Token::Plus);
        assert_eq!(tokens[2].token, Token::NumberLiteral(2.0f64));
    }

    #[test]
    fn expression_with_whitespaces() {
        let (tokens, _) = scan(&"1 + 2");
        assert_eq!(tokens[0].token, Token::NumberLiteral(1.0f64));
        assert_eq!(tokens[1].token, Token::Plus);
        assert_eq!(tokens[2].token, Token::NumberLiteral(2.0f64));
    }

    #[test]
    fn assignement_with_comment() {
        let (tokens, _) = scan(&"var a = 1.0; // A comment");
        assert_eq!(tokens[0].token, Token::Var);
        assert_eq!(tokens[1].token, Token::Identifier("a".into()));
        assert_eq!(tokens[2].token, Token::Equal);
        assert_eq!(tokens[3].token, Token::NumberLiteral(1.0f64));
        assert_eq!(tokens[4].token, Token::Semicolon);
    }

    #[test]
    fn multiline_statements() {
        let (tokens, _) = scan(
            &r#"var a = 1.0;
                var b = "Hello";"#,
        );
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
        assert_eq!(tokens[1].position.line, 1);
        assert_eq!(tokens[1].position.column, 5);
        assert_eq!(tokens[9].position.line, 2);
        assert_eq!(tokens[9].position.column, 32); // There are 16 spaces before the text!
    }

    #[test]
    fn expression_with_bad_character() {
        let (tokens, errors) = scan(&"1 + $2");
        assert_eq!(tokens[0].token, Token::NumberLiteral(1.0f64));
        assert_eq!(tokens[1].token, Token::Plus);
        assert_eq!(tokens[2].token, Token::NumberLiteral(2.0f64));
        assert_eq!(errors.len(), 1);
    }

    #[test]
    fn statements_with_unterminated_string() {
        let (tokens, errors) = scan(
            &r#"var a = "Unterminated;
                var b = "Hello";"#,
        );
        assert_eq!(tokens[0].token, Token::Var);
        assert_eq!(tokens[1].token, Token::Identifier("a".into()));
        assert_eq!(tokens[2].token, Token::Equal);
        // Literal not reported, it wasn't terminated
        // Semicolon got embedded into the string
        assert_eq!(tokens[3].token, Token::Var);
        assert_eq!(tokens[4].token, Token::Identifier("b".into()));
        assert_eq!(tokens[5].token, Token::Equal);
        assert_eq!(tokens[6].token, Token::StringLiteral("Hello".into()));
        assert_eq!(tokens[7].token, Token::Semicolon);
        assert_eq!(errors.len(), 1);
    }

}
