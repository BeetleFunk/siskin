pub fn scan_tokens(code: &str) -> Vec<Token> {
    let mut tokens: Vec<Token> = vec![];

    let code_chars: Vec<char> = code.chars().collect();
    let mut cursor = Cursor::new(code_chars);

    loop {
        let token = next_token(&mut cursor);
        tokens.push(token);

        if matches!(tokens.last().unwrap().token_type, TokenType::EOF) {
            break tokens
        }
    }
}

#[derive(Debug)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: u32,
}

#[derive(Debug)]
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
    Identifier(String),
    String(String),
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

    // End-Of-File
    EOF,
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
}

fn next_token(cursor: &mut Cursor) -> Token {
    if let Some(current) = cursor.take() {
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
            '"' => {
                cursor.buffer_next_until(|next| next == '"');
                if cursor.buffer_next_if_match('"') {
                    let quoted = cursor.string_at_cursor();
                    // TODO increment line count if newlines within string
                    Token {
                        // lexeme bounded by quote chars so this slice should never panic
                        token_type: TokenType::String(quoted[1..quoted.len()-1].to_string()),
                        lexeme: quoted,
                        line: cursor.line,
                    }
                } else {
                    panic!("Unterminated string at line {0}", cursor.line)
                }
            }
            _ => {
                if current.is_alphabetic() {
                    scan_identifier(cursor)
                } else if current.is_digit(10) {
                    scan_number(cursor)
                } else {
                    panic!("Unexpected character at line {0}", cursor.line)
                }
            }
        }
    } else {
        Token {
            token_type: TokenType::EOF,
            lexeme: String::from(""),
            line: cursor.line,
        }
    }
}

fn token_at_cursor(cursor: &Cursor, token_type: TokenType) -> Token {
    Token {
        token_type,
        lexeme: cursor.string_at_cursor(),
        line: cursor.line,
    }
}

fn scan_identifier(cursor: &mut Cursor) -> Token {
    cursor.buffer_next_until(|next| !next.is_alphanumeric());
    let lexeme = cursor.string_at_cursor();

    let token_type = if let Some(keyword) = to_keyword(&lexeme) {
        keyword
    } else {
        TokenType::Identifier(lexeme.clone())
    };

    Token {
        token_type,
        lexeme,
        line: cursor.line,
    }
}

fn scan_number(cursor: &mut Cursor) -> Token {
    let is_not_digit = |next: char| !next.is_digit(10);
    cursor.buffer_next_until(is_not_digit);

    // pickup fractional part if necessary
    if cursor.buffer_next_if_match('.') {
        cursor.buffer_next_until(is_not_digit);
    }

    let lexeme = cursor.string_at_cursor();

    // TODO return result with error
    let number: f64 = lexeme.parse().unwrap();

    Token {
        token_type: TokenType::Number(number),
        lexeme: lexeme,
        line: cursor.line,
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
