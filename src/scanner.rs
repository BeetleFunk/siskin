pub fn scan_tokens(code: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = vec![];

    println!("scanned {code}");

    let scanner = Scanner {
        code,
        cursor_begin: 0,
        cursor_end: 0,
        line: 1,
    };

    let mut eof = false;

    while !eof {
        let token = scanner.next_token();

        if let TokenType::EOF = token.token_type {
            eof = true;
        }

        tokens.push(scanner.next_token());
    }

    tokens
}

#[derive(Debug)]
struct Scanner<'a> {
    code: &'a str,
    cursor_begin: u32,
    cursor_end: u32,
    line: u32,
}

impl<'a> Scanner<'a> {
    fn next_token(&self) -> Token<'a> {
        Token {
            token_type: TokenType::EOF,
            lexeme: "fake1".to_string(),
            line: 6,
        }
    }
}

#[derive(Debug)]
pub struct Token<'a> {
    pub token_type: TokenType<'a>,
    pub lexeme: String,
    pub line: u32,
}

#[derive(Debug)]
pub enum TokenType<'a> {
    // Single-character tokens.
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

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier(&'a str),
    String(&'a str),
    Number(f64),

    // Keywords.
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

    EOF,
}
