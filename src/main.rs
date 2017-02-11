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

    fn advance(&mut self) -> char {
        self.current += 1;
        self.source.chars().nth(self.current - 1).unwrap()
    }

    fn add_simple_context(&self, token: Token) -> TokenWithContext {
        TokenWithContext {
            token: token,
            lexeme: self.source.chars().skip(self.start).take(self.current - self.start).collect(),
            line: self.line,
        }
    }

    fn scan_next(&mut self) -> TokenWithContext {
        self.start = self.current;
        match self.advance() {
            '(' => self.add_simple_context(Token::LeftParen),
            ')' => self.add_simple_context(Token::RightParen),
            '{' => self.add_simple_context(Token::LeftBrace),
            '}' => self.add_simple_context(Token::RightBrace),
            ',' => self.add_simple_context(Token::Comma),
            '.' => self.add_simple_context(Token::Dot),
            '-' => self.add_simple_context(Token::Minus),
            '+' => self.add_simple_context(Token::Plus),
            ';' => self.add_simple_context(Token::Semicolon),
            '*' => self.add_simple_context(Token::Star),
            _ => unimplemented!(),
        }
    }
}


fn tokenize(source: &String) -> Result<Vec<TokenWithContext>, String> {
    let mut scanner = Scanner::initialize(source);
    let mut tokens = Vec::new();
    while !scanner.is_at_end() {
        tokens.push(scanner.scan_next());
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
