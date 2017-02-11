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
    NumberLiteral(u32),
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
    Comment,
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

    fn peek(&self) -> char() {
        self.char_at(self.current)
    }

    fn advance(&mut self) -> char {
        self.current += 1;
        self.char_at(self.current - 1)
    }

    fn is_match(&mut self, expected: char) -> bool {
        if (self.is_at_end()) {
            return false;
        }
        if (self.char_at(self.current) != expected) {
            return false;
        }
        self.current += 1;
        return true;
    }

    fn add_context(&self, token: Token) -> TokenWithContext {
        TokenWithContext {
            token: token,
            lexeme: self.source.chars().skip(self.start).take(self.current - self.start).collect(),
            line: self.line,
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
                    while (self.peek() != '\n' && !self.is_at_end()) {
                        self.advance();
                    }
                    Token::Comment
                } else {
                    Token::Slash
                }
            }
            _ => {
                return Err(format!("Unexpected character at line {}, pos {}",
                                   self.line,
                                   self.current))
            }
        };
        Ok(self.add_context(token))
    }
}


fn tokenize(source: &String) -> Result<Vec<TokenWithContext>, String> {
    let mut scanner = Scanner::initialize(source);
    let mut tokens = Vec::new();
    while !scanner.is_at_end() {
        match scanner.scan_next() {
            Ok(token) => tokens.push(token),
            Err(message) => return Err(message),
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
    loop {
        print!("> ");
        io::stdout().flush();
        let mut source = String::new();
        io::stdin().read_line(&mut source);
        try!(run(&source));
        ()
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
