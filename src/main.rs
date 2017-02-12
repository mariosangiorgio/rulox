use std::env;
use std::fs::File;
use std::io;
use std::io::prelude::*;

#[derive(Debug)]
enum Token {
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
    // These are not described in the book but they simplify the parser
    Comment,
    Whitespace,
}

#[derive(Debug)]
struct TokenWithContext {
    token: Token,
    lexeme: String, // TODO: make a reference
    line: usize,
}

struct Scanner {
    start: usize,
    current: usize,
    line: usize,
    source: String, // TODO: make a reference
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

impl Scanner {
    fn initialize(source: &String) -> Scanner {
        Scanner {
            start: 0,
            current: 0,
            line: 0,
            source: source.clone(),
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn char_at(&self, index: usize) -> char {
        // TODO: there must be a better way
        self.source.chars().nth(index).unwrap()
    }

    fn substring(&self, start: usize, end: usize) -> String {
        self.source.chars().skip(start).take(end - start).collect()
    }

    fn peek(&self) -> char() {
        if self.is_at_end() {
            '\0'
        } else {
            self.char_at(self.current)
        }
    }

    fn peek_next(&self) -> char() {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.char_at(self.current + 1)
        }
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.char_at(self.current - 1)
    }

    fn is_match(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.char_at(self.current) != expected {
            return false;
        }
        self.current += 1;
        return true;
    }

    fn add_context(&self, token: Token) -> TokenWithContext {
        TokenWithContext {
            token: token,
            lexeme: self.substring(self.start, self.current),
            line: self.line,
        }
    }

    fn string(&mut self) -> Result<Token, String> {
        let initial_line = self.line;
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1
            }
            self.advance();
        }
        if self.is_at_end() {
            return Err(format!("Unterminated string at line {}", initial_line));
        }
        self.advance();
        Ok(Token::StringLiteral(self.substring(self.start + 1, self.current - 1)))
    }

    fn number(&mut self) -> Token {
        while is_digit(self.peek()) {
            self.advance();
        }
        if self.peek() == '.' && is_digit(self.peek_next()) {
            self.advance(); // Consume the .
            while is_digit(self.peek()) {
                self.advance();
            }
        }
        let literal = self.substring(self.start, self.current);
        let value = literal.parse::<f64>().unwrap();
        Token::NumberLiteral(value)
    }

    fn identifier(&mut self) -> Token {
        while is_alphanumeric(self.peek()) {
            self.advance();
        }
        // TODO: take a ref in the first place
        match self.substring(self.start, self.current).as_ref() {
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
        self.start = self.current;
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
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                    Token::Comment
                } else {
                    Token::Slash
                }
            }
            ' ' => Token::Whitespace,
            '\r' => Token::Whitespace,
            '\t' => Token::Whitespace,
            '\n' => {
                self.line += 1;
                Token::Whitespace
            }
            '"' => try!(self.string()),
            c => {
                if is_digit(c) {
                    self.number()
                } else if is_alpha(c) {
                    self.identifier()
                } else {
                    let position = self.current - 1;
                    return Err(format!("Unexpected character {} at line {}, pos {}",
                                       self.char_at(position),
                                       self.line,
                                       position));
                }
            }
        };
        Ok(self.add_context(token))
    }
}


fn tokenize(source: &String) -> Result<Vec<TokenWithContext>, String> {
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

fn run(source: &String) -> Result<(), String> {
    let tokens = try!(tokenize(source));
    for token in tokens {
        println!("{:?}", token);
    }
    Ok(())
}

fn run_file(file_name: &String) -> Result<(), String> {
    match File::open(file_name) {
        Err(_) => {
            Err("Error opening file".into()) // TODO: add context
        }
        Ok(mut file) => {
            let mut source = String::new();
            match file.read_to_string(&mut source) {
                Err(_) => {
                    Err("Error reading file".into()) // TODO: add context
                }
                Ok(_) => run(&source),
            }
        }
    }
}

fn run_prompt() -> Result<(), String> {
    println!("Rulox - A lox interpreter written in Rust");
    let _ = io::stdout().flush(); //TODO: is this okay?
    loop {
        print!("> ");
        let _ = io::stdout().flush();
        let mut source = String::new();
        let _ = io::stdin().read_line(&mut source);
        //TODO: add a way to exit
        try!(run(&source))
        //TODO: report syntax errors to the user
    }
}

fn main() {
    let args: Vec<String> = env::args().collect();
    let result = match args.len() {
        // The first argument is the program name
        1 => run_prompt(),
        2 => run_file(&args[1]),
        _ => {
            println!("Usage: rulox [script]");
            Ok(())
        }
    };
    let exit_code = match result {
        Ok(_) => 0,
        Err(e) => {
            println!("{}", e);
            1
        }
    };
    std::process::exit(exit_code)
}
