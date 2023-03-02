pub fn scan_tokens(code: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = vec![];

    println!("scanned {code}");

    let code_chars: Vec<char> = code.chars().collect();
    let mut cursor = Cursor::new(code_chars);

    let mut eof = false;
    while !eof {
        let token = next_token(&mut cursor);

        if let TokenType::EOF = token.token_type {
            eof = true;
        }

        tokens.push(token);
    }

    return tokens;
}

#[derive(Debug)]
struct Cursor {
    code: Vec<char>,
    begin: usize,
    end: usize,
    line: u32,
}

impl Cursor {
    fn new(code: Vec<char>) -> Cursor {
        Cursor {
            code,
            begin: 0,
            end: 0,
            line: 1,
        }
    }

    // advance to the next char if not at the end of the Vec
    fn take(&mut self) -> Option<char> {
        if self.end < self.code.len() {
            self.begin = self.end;
            self.end = self.begin + 1;
            Option::Some(self.code[self.begin])
        } else {
            Option::None
        }
    }

    // increment the end of the cursor if condition is met
    fn buffer_next_if_true(&mut self, condition: impl Fn(char) -> bool) -> bool {
        if self.end < self.code.len() && condition(self.code[self.end]) {
            self.end += 1;
            true
        } else {
            false
        }
    }

    fn buffer_next_until(&mut self, condition: impl Fn(char) -> bool) {
        let negation = |next| !condition(next);
        while self.buffer_next_if_true(negation) {};
    }

    fn buffer_next_if_match(&mut self, c: char) -> bool {
        self.buffer_next_if_true(|next| next == c)
    }

    fn string_at_cursor(&self) -> String {
        let chars = &self.code[self.begin..self.end];
        String::from_iter(chars)
    }
}

fn next_token<'a>(cursor: &mut Cursor) -> Token<'a> {
    if let Some(current) = cursor.take() {
        println!("scanning char {0}", current);

        match current {
            '(' => token_at_cursor(cursor, TokenType::LeftParen),
            ')' => token_at_cursor(cursor, TokenType::RightParen),
            '{' => token_at_cursor(cursor, TokenType::LeftBrace),
            '}' => token_at_cursor(cursor, TokenType::RightBrace),
            ',' => token_at_cursor(cursor, TokenType::Comma),
            '.' => token_at_cursor(cursor, TokenType::Dot),
            '-' => token_at_cursor(cursor, TokenType::Minus),
            '+' => token_at_cursor(cursor, TokenType::Plus),
            ';' => token_at_cursor(cursor, TokenType::Semicolon),
            '*' => token_at_cursor(cursor, TokenType::Star),
            '!' => {
                if cursor.buffer_next_if_match('=') {
                    token_at_cursor(cursor, TokenType::BangEqual)
                } else {
                    token_at_cursor(cursor, TokenType::Bang)
                }
            }
            '=' => {
                if cursor.buffer_next_if_match('=') {
                    token_at_cursor(cursor, TokenType::EqualEqual)
                } else {
                    token_at_cursor(cursor, TokenType::Equal)
                }
            }
            '<' => {
                if cursor.buffer_next_if_match('=') {
                    token_at_cursor(cursor, TokenType::LessEqual)
                } else {
                    token_at_cursor(cursor, TokenType::Less)
                }
            }
            '>' => {
                if cursor.buffer_next_if_match('=') {
                    token_at_cursor(cursor, TokenType::GreaterEqual)
                } else {
                    token_at_cursor(cursor, TokenType::Greater)
                }
            }
            '/' => {
                if cursor.buffer_next_if_match('/') {
                    // consume until end of line but don't also consume the newline
                    cursor.buffer_next_until(|next| next == '\n');
                    // next token should be either the newline or end-of-file
                    next_token(cursor)
                } else {
                    token_at_cursor(cursor, TokenType::Slash)
                }
            }
            ' ' | '\r' | '\t' => next_token(cursor), // Ignore whitespace
            '\n' => {
                cursor.line += 1;
                next_token(cursor)
            }
            _ => panic!("Unexpected character"),
        }
    } else {
        Token {
            token_type: TokenType::EOF,
            lexeme: String::from(""),
            line: cursor.line,
        }
    }
}

fn token_at_cursor<'a>(cursor: &Cursor, token_type: TokenType<'a>) -> Token<'a> {
    Token {
        token_type,
        lexeme: cursor.string_at_cursor(),
        line: cursor.line,
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
