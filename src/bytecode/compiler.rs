use std::mem;
use std::result;

use crate::error::BasicError;
use crate::scanner::{Scanner, Token, TokenType};

use super::code::{Chunk, OpCode};

type UnitResult = result::Result<(), BasicError>;

struct Parser {
    scanner: Scanner,
    current: Token,
    previous: Option<Token>,
}

impl Parser {
    // automatically prime current with the first token, hence the possible error
    fn new(mut scanner: Scanner) -> result::Result<Self, BasicError> {
        let current = scanner.next_token()?;
        Ok(Parser {
            scanner,
            current,
            previous: None,
        })
    }

    fn advance(&mut self) -> result::Result<&Token, BasicError> {
        let replaced = mem::replace(&mut self.current, self.scanner.next_token()?);
        self.previous = Some(replaced);
        Ok(&self.current)
    }
}

// parsing precedence values
const PREC_NONE: u32 = 0;
const PREC_ASSIGNMENT: u32 = 1; // =
const PREC_OR: u32 = 2; // or
const PREC_AND: u32 = 3; // and
const PREC_EQUALITY: u32 = 4; // == !=
const PREC_COMPARISON: u32 = 5; // < > <= >=
const PREC_TERM: u32 = 6; // + -
const PREC_FACTOR: u32 = 7; // * /
const PREC_UNARY: u32 = 8; // ! -
const PREC_CALL: u32 = 9; // . ()

struct ParseRule {
    prefix: Option<fn(&mut Compiler) -> UnitResult>,
    infix: Option<fn(&mut Compiler) -> UnitResult>,
    precedence: u32,
}

// Each entry includes corresponding TokenType either for index sorting or for sanity checking if table is used directly
#[cfg_attr(rustfmt, rustfmt_skip)]
const PARSE_TABLE: [(TokenType, ParseRule); 39] = [
    (TokenType::LeftParen,      ParseRule { prefix: Some(grouping), infix: None,            precedence: PREC_NONE }),
    (TokenType::RightParen,     ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::LeftBrace,      ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }), 
    (TokenType::RightBrace,     ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Comma,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Dot,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Minus,          ParseRule { prefix: Some(unary),    infix: Some(binary),    precedence: PREC_TERM }),
    (TokenType::Plus,           ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_TERM }),
    (TokenType::Semicolon,      ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Slash,          ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_FACTOR }),
    (TokenType::Star,           ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_FACTOR }),
    (TokenType::Bang,           ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::BangEqual,      ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Equal,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::EqualEqual,     ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Greater,        ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::GreaterEqual,   ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Less,           ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::LessEqual,      ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Identifier,     ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::String,         ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Number,         ParseRule { prefix: Some(number),   infix: None,            precedence: PREC_NONE }),
    (TokenType::And,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Class,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Else,           ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::False,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::For,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Fun,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::If,             ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Nil,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Or,             ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Print,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Return,         ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Super,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::This,           ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::True,           ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Var,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::While,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Eof,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE })];

fn get_rule(token_type: TokenType) -> &'static ParseRule {
    &PARSE_TABLE[token_type as usize].1
}

pub fn compile(source: &str) -> result::Result<Chunk, BasicError> {
    let mut scanner = Scanner::new(source);
    let mut parser = Parser::new(scanner)?;

    let mut chunk = Chunk::new();

    // TBD: Should compiler own these or not?
    let mut compiler = Compiler { parser, chunk };

    let func = PARSE_TABLE[0].1.prefix.unwrap();
    func(&mut compiler);
    loop {
        // This should always be the last token?
        if compiler.parser.current.token_type == TokenType::Eof {
            break;
        } else {
            panic!("Expect EOF token type as the final scanner token.")
        }
    }

    compiler.emit_byte(OpCode::Return);

    Ok(compiler.chunk)
}

struct Compiler {
    parser: Parser,
    chunk: Chunk,
}

impl Compiler {
    fn new(parser: Parser, chunk: Chunk) -> Compiler {
        Compiler { parser, chunk }
    }

    fn emit_byte(&mut self, byte: OpCode) {
        self.chunk
            .write_op(byte, self.parser.previous.as_ref().unwrap().line);
    }

    fn emit_bytes(&mut self, byte1: OpCode, byte2: u8) {
        self.chunk
            .write_op(byte1, self.parser.previous.as_ref().unwrap().line);
        self.chunk
            .write(byte2, self.parser.previous.as_ref().unwrap().line);
    }

    fn consume(
        &mut self,
        expected: TokenType,
        error_msg: &str,
    ) -> result::Result<&Token, BasicError> {
        let token = self.parser.advance()?;
        if token.token_type == expected {
            Ok(token)
        } else {
            Err(build_error(error_msg, token.line))
        }
    }
}

fn expression(compiler: &mut Compiler) {
    parse_precedence(compiler, PREC_ASSIGNMENT)
}

fn parse_precedence(compiler: &mut Compiler, precedence: u32) {}

fn number(compiler: &mut Compiler) -> UnitResult {
    let token = compiler.parser.previous.as_ref().unwrap();
    if token.token_type == TokenType::Number {
        let address = compiler.chunk.add_constant(token.extract_number().into());
        compiler.emit_bytes(OpCode::Constant, address);
    } else {
        panic!("Token at location is not a number.");
    }
    Ok(())
}

fn grouping(compiler: &mut Compiler) -> UnitResult {
    expression(compiler);
    compiler.consume(TokenType::RightParen, "Expect ')' after expression.")?;
    Ok(())
}

fn unary(compiler: &mut Compiler) -> UnitResult {
    let token_type = compiler
        .parser
        .previous
        .as_ref()
        .unwrap()
        .token_type
        .clone();
    // compile operand
    parse_precedence(compiler, PREC_UNARY);
    match token_type {
        TokenType::Minus => compiler.emit_byte(OpCode::Negate),
        _ => panic!("Unhandled unary token: {:?}", token_type),
    }
    Ok(())
}

fn binary(compiler: &mut Compiler) -> UnitResult {
    let token_type = compiler.parser.previous.as_ref().unwrap().token_type;
    let rule = get_rule(token_type);
    parse_precedence(compiler, rule.precedence + 1);
    match token_type {
        TokenType::Plus => compiler.emit_byte(OpCode::Add),
        TokenType::Minus => compiler.emit_byte(OpCode::Subtract),
        TokenType::Star => compiler.emit_byte(OpCode::Multiply),
        TokenType::Slash => compiler.emit_byte(OpCode::Divide),
        _ => panic!("Unhandled binary token: {:?}", token_type),
    }
    Ok(())
}

fn build_error(message: &str, line: u32) -> BasicError {
    BasicError::new(&format!("Compilation error at line {line}: {message}"))
}
