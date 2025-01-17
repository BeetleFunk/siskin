use std::result;

use crate::error::BasicError;

type TokenResult = result::Result<Token, BasicError>;

// wrapper struct that manages the cursor and provides the next token on demand
pub struct Scanner {
    cursor: Cursor,
}

impl Scanner {
    pub fn new(code: &str) -> Self {
        let code_chars: Vec<char> = code.chars().collect();
        Scanner {
            cursor: Cursor::new(code_chars),
        }
    }

    // TODO: returns EOF every time this is called upon reaching the end of the code, change to Option::None in order to force the caller to handle this?
    pub fn next_token(&mut self) -> result::Result<Token, BasicError> {
        next_token(&mut self.cursor)
    }
}

pub fn scan_tokens(code: &str) -> result::Result<Vec<Token>, BasicError> {
    let mut tokens: Vec<Token> = Vec::new();
    let mut scanner = Scanner::new(code);

    loop {
        tokens.push(scanner.next_token()?);
        if tokens.last().unwrap().token_type == TokenType::Eof {
            break Ok(tokens);
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: u32,
    pub token_value: Option<TokenValue>,
}

impl Token {
    fn new(token_type: TokenType, lexeme: String, line: u32) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            token_value: None,
        }
    }

    fn with_value(token_type: TokenType, lexeme: String, line: u32, token_value: TokenValue) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            token_value: Some(token_value),
        }
    }

    pub fn extract_number(&self) -> f64 {
        if matches!(self.token_type, TokenType::Number) {
            if let Some(TokenValue::Number(value)) = self.token_value {
                value
            } else {
                panic!("Number token did not have a numeric value");
            }
        } else {
            panic!("Called extract_number on non-number token");
        }
    }

    pub fn extract_string(&self) -> &String {
        if matches!(self.token_type, TokenType::String) {
            if let Some(TokenValue::String(value)) = &self.token_value {
                value
            } else {
                panic!("String token did not have a string value");
            }
        } else {
            panic!("Called extract_string on non-string token");
        }
    }

    pub fn into_name(self) -> String {
        if matches!(self.token_type, TokenType::Identifier) {
            if let Some(TokenValue::Name(value)) = self.token_value {
                value
            } else {
                panic!("Identifier token did not have a name value");
            }
        } else {
            panic!("Called into_name on non-identifier token");
        }
    }

    pub fn extract_name(&self) -> &String {
        if matches!(self.token_type, TokenType::Identifier) {
            if let Some(TokenValue::Name(value)) = &self.token_value {
                value
            } else {
                panic!("Identifier token did not have a name value");
            }
        } else {
            panic!("Called extract_name on non-identifier token");
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
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
    Identifier,
    String,
    Number,

    // Keywords.
    And,
    Class,
    Else,
    False,
    For,
    Fun,
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

    // End-Of-File
    Eof,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokenValue {
    Name(String),
    Number(f64),
    String(String),
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

    // advance to the next char (after end index) if not at the end of the Vec
    fn take(&mut self) -> Option<char> {
        if self.end < self.code.len() {
            self.begin = self.end;
            self.end = self.begin + 1;
            Option::Some(self.code[self.begin])
        } else {
            Option::None
        }
    }

    // increment the end of the cursor if the condition is met and the index is not at the end of the Vec
    fn buffer_next_if_true(&mut self, condition: impl Fn(char) -> bool) -> bool {
        if self.end < self.code.len() && condition(self.code[self.end]) {
            self.end += 1;
            true
        } else {
            false
        }
    }

    fn buffer_next_if_match(&mut self, c: char) -> bool {
        self.buffer_next_if_true(|next| next == c)
    }

    // increment the end of the cursor until the condition is met or the end of the Vec is reached
    // buffer will not include the char for which the condition is true
    fn buffer_next_until(&mut self, condition: impl Fn(char) -> bool) {
        let negation = |next| !condition(next);
        while self.buffer_next_if_true(negation) {}
    }

    fn string_at_cursor(&self) -> String {
        let chars = &self.code[self.begin..self.end];
        String::from_iter(chars)
    }

    fn chars_at_cursor(&self) -> &[char] {
        &self.code[self.begin..self.end]
    }
}

fn next_token(cursor: &mut Cursor) -> TokenResult {
    if let Some(current) = cursor.take() {
        if current.is_whitespace() {
            // increment line count for each newline, ignore all other whitespace
            if current == '\n' {
                cursor.line += 1;
            }
            next_token(cursor)
        } else {
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
                '"' => {
                    cursor.buffer_next_until(|next| next == '"');
                    if cursor.buffer_next_if_match('"') {
                        // increment line count for any newline characters within the string but keep track of original line number
                        let starting_line = cursor.line;
                        cursor.line += cursor.chars_at_cursor().iter().filter(|&c| *c == '\n').count() as u32;

                        let quoted = cursor.string_at_cursor();
                        Ok(Token {
                            // lexeme bounded by quote chars so this slice should never panic
                            token_type: TokenType::String,
                            lexeme: quoted.clone(),
                            line: starting_line,
                            token_value: Some(TokenValue::String(quoted[1..(quoted.len() - 1)].to_string())),
                        })
                    } else {
                        Err(build_error("Unterminated string.", cursor.line))
                    }
                }
                _ => {
                    if current.is_alphabetic() {
                        scan_identifier(cursor)
                    } else if current.is_ascii_digit() {
                        scan_number(cursor)
                    } else {
                        Err(build_error(&format!("Unexpected character '{current}'."), cursor.line))
                    }
                }
            }
        }
    } else {
        Ok(Token::new(TokenType::Eof, String::from(""), cursor.line))
    }
}

// creates a token without any TokenValue at the current cursor range
fn token_at_cursor(cursor: &Cursor, token_type: TokenType) -> TokenResult {
    Ok(Token::new(token_type, cursor.string_at_cursor(), cursor.line))
}

fn scan_identifier(cursor: &mut Cursor) -> TokenResult {
    cursor.buffer_next_until(|next| !next.is_alphanumeric());
    let lexeme = cursor.string_at_cursor();

    if let Some(keyword) = to_keyword(&lexeme) {
        Ok(Token::new(keyword, lexeme, cursor.line))
    } else {
        Ok(Token::with_value(
            TokenType::Identifier,
            lexeme.clone(),
            cursor.line,
            TokenValue::Name(lexeme),
        ))
    }
}

fn scan_number(cursor: &mut Cursor) -> TokenResult {
    let is_not_digit = |next: char| !next.is_ascii_digit();

    cursor.buffer_next_until(is_not_digit);

    // pickup fractional part if necessary
    if cursor.buffer_next_if_match('.') {
        cursor.buffer_next_until(is_not_digit);
    }

    let lexeme = cursor.string_at_cursor();

    match lexeme.parse() {
        Ok(number) => Ok(Token::with_value(
            TokenType::Number,
            lexeme,
            cursor.line,
            TokenValue::Number(number),
        )),
        Err(e) => Err(build_error(&e.to_string(), cursor.line)),
    }
}

fn to_keyword(identifier: &str) -> Option<TokenType> {
    match identifier {
        "and" => Option::Some(TokenType::And),
        "class" => Option::Some(TokenType::Class),
        "else" => Option::Some(TokenType::Else),
        "false" => Option::Some(TokenType::False),
        "for" => Option::Some(TokenType::For),
        "fun" => Option::Some(TokenType::Fun),
        "if" => Option::Some(TokenType::If),
        "nil" => Option::Some(TokenType::Nil),
        "or" => Option::Some(TokenType::Or),
        "print" => Option::Some(TokenType::Print),
        "return" => Option::Some(TokenType::Return),
        "super" => Option::Some(TokenType::Super),
        "this" => Option::Some(TokenType::This),
        "true" => Option::Some(TokenType::True),
        "var" => Option::Some(TokenType::Var),
        "while" => Option::Some(TokenType::While),
        _ => Option::None,
    }
}

fn build_error(message: &str, line: u32) -> BasicError {
    BasicError::new(&format!("Scan error at line {line}: {message}"))
}
