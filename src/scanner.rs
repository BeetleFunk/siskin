pub fn scan_tokens(code: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = vec![];

    println!("scanned {code}");

    let mut scanner = Scanner {
        chars: code.chars().collect(),
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

        tokens.push(token);
    }

    return tokens;
}

#[derive(Debug)]
struct Scanner {
    chars: Vec<char>,
    cursor_begin: usize,
    cursor_end: usize,
    line: u32,
}

impl Scanner {
    fn next_token<'a>(&mut self) -> Token<'a> {
        // TODO: bounds check
        let current = self.chars[self.cursor_begin];

        self.line += 1;

        match current {
            '(' => Token {
                token_type: TokenType::LeftParen,
                lexeme: String::from(current),
                line: self.line,
            },
            _ => panic!("boom"),
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
