use std::mem;
use std::result;

use crate::error::BasicError;
use crate::scanner::{Scanner, Token, TokenType};

use super::code::{self, Chunk, OpCode};

type UnitResult = result::Result<(), BasicError>;

static DEBUG_DUMP_CHUNK: bool = true;

struct Parser {
    scanner: Scanner,
    current: Token,
    previous: Token,
}

impl Parser {
    // automatically prime current with the first token, hence the possible error
    fn new(mut scanner: Scanner) -> result::Result<Self, BasicError> {
        let current = scanner.next_token()?;
        Ok(Parser {
            scanner,
            current,
            previous: Token {
                // this initial dummy token should never actually be read with look-ahead parsing
                token_type: TokenType::Eof,
                lexeme: "".to_string(),
                line: 0,
                token_value: None,
            },
        })
    }

    fn advance(&mut self) -> result::Result<&Token, BasicError> {
        let replaced = mem::replace(&mut self.current, self.scanner.next_token()?);
        self.previous = replaced;
        Ok(&self.current)
    }
}

// parsing precedence values
const PREC_NONE: u32 = 0;
const PREC_ASSIGNMENT: u32 = 1; // =
//const PREC_OR: u32 = 2; // or
//const PREC_AND: u32 = 3; // and
const PREC_EQUALITY: u32 = 4; // == !=
const PREC_COMPARISON: u32 = 5; // < > <= >=
const PREC_TERM: u32 = 6; // + -
const PREC_FACTOR: u32 = 7; // * /
const PREC_UNARY: u32 = 8; // ! -
//const PREC_CALL: u32 = 9; // . ()

struct ParseRule {
    prefix: Option<fn(&mut Compiler) -> UnitResult>,
    infix: Option<fn(&mut Compiler) -> UnitResult>,
    precedence: u32,
}

// Each entry includes corresponding TokenType either for index sorting or for sanity checking if table is used directly
#[rustfmt::skip]
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
    (TokenType::Bang,           ParseRule { prefix: Some(unary),    infix: None,            precedence: PREC_NONE }),
    (TokenType::BangEqual,      ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_EQUALITY }),
    (TokenType::Equal,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::EqualEqual,     ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_EQUALITY }),
    (TokenType::Greater,        ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_COMPARISON }),
    (TokenType::GreaterEqual,   ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_COMPARISON }),
    (TokenType::Less,           ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_COMPARISON }),
    (TokenType::LessEqual,      ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_COMPARISON }),
    (TokenType::Identifier,     ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::String,         ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Number,         ParseRule { prefix: Some(number),   infix: None,            precedence: PREC_NONE }),
    (TokenType::And,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Class,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Else,           ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::False,          ParseRule { prefix: Some(literal),  infix: None,            precedence: PREC_NONE }),
    (TokenType::For,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Fun,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::If,             ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Nil,            ParseRule { prefix: Some(literal),  infix: None,            precedence: PREC_NONE }),
    (TokenType::Or,             ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Print,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Return,         ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Super,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::This,           ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::True,           ParseRule { prefix: Some(literal),  infix: None,            precedence: PREC_NONE }),
    (TokenType::Var,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::While,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Eof,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE })];

fn get_rule(token_type: TokenType) -> &'static ParseRule {
    &PARSE_TABLE[token_type as usize].1
}

pub fn compile(source: &str) -> result::Result<Chunk, BasicError> {
    let scanner = Scanner::new(source);
    let parser = Parser::new(scanner)?;

    let chunk = Chunk::new();

    // TBD: Should compiler own these or not?
    let mut compiler = Compiler::new(parser, chunk);

    expression(&mut compiler)?;

    // This should always be the last token?
    if compiler.parser.current.token_type != TokenType::Eof {
        panic!("Expect EOF token type as the final scanner token.")
    }

    compiler.emit_op(OpCode::Return);

    if DEBUG_DUMP_CHUNK {
        code::disassemble_chunk(&compiler.chunk, "name");
    }

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

    fn emit_op(&mut self, byte: OpCode) {
        self.chunk.write_op(byte, self.parser.previous.line);
    }

    fn emit_ops(&mut self, byte1: OpCode, byte2: OpCode) {
        self.chunk.write_op(byte1, self.parser.previous.line);
        self.chunk.write_op(byte2, self.parser.previous.line);
    }

    fn emit_data_op(&mut self, byte1: OpCode, byte2: u8) {
        self.chunk.write_op(byte1, self.parser.previous.line);
        self.chunk.write(byte2, self.parser.previous.line);
    }

    fn consume(&mut self, expected: TokenType, error_msg: &str) -> UnitResult {
        if self.parser.current.token_type == expected {
            self.parser.advance()?;
            Ok(())
        } else {
            Err(build_error(error_msg, self.parser.current.line))
        }
    }
}

fn expression(compiler: &mut Compiler) -> UnitResult {
    parse_precedence(compiler, PREC_ASSIGNMENT)
}

fn parse_precedence(compiler: &mut Compiler, precedence: u32) -> UnitResult {
    compiler.parser.advance()?;
    let prefix_rule = get_rule(compiler.parser.previous.token_type).prefix;
    let prefix_rule = prefix_rule
        .ok_or_else(|| build_error("Expect expression.", compiler.parser.previous.line))?;
    prefix_rule(compiler)?;

    while precedence <= get_rule(compiler.parser.current.token_type).precedence {
        compiler.parser.advance()?;
        let infix_rule = get_rule(compiler.parser.previous.token_type).infix.unwrap();
        infix_rule(compiler)?;
    }

    Ok(())
}

fn number(compiler: &mut Compiler) -> UnitResult {
    let token = &compiler.parser.previous;
    if token.token_type == TokenType::Number {
        let address = compiler.chunk.add_constant(token.extract_number().into());
        compiler.emit_data_op(OpCode::Constant, address);
    } else {
        panic!("Token at line {} is not a number.", token.line);
    }
    Ok(())
}

fn literal(compiler: &mut Compiler) -> UnitResult {
    match compiler.parser.previous.token_type {
        TokenType::Nil => compiler.emit_op(OpCode::Nil),
        TokenType::True => compiler.emit_op(OpCode::True),
        TokenType::False => compiler.emit_op(OpCode::False),
        _ => panic!(
            "Token at line {} is not a literal.",
            compiler.parser.previous.line
        ),
    }
    Ok(())
}

fn grouping(compiler: &mut Compiler) -> UnitResult {
    expression(compiler)?;
    compiler.consume(TokenType::RightParen, "Expect ')' after expression.")?;
    Ok(())
}

fn unary(compiler: &mut Compiler) -> UnitResult {
    let token_type = compiler.parser.previous.token_type;
    // compile operand
    parse_precedence(compiler, PREC_UNARY)?;
    match token_type {
        TokenType::Bang => compiler.emit_op(OpCode::Not),
        TokenType::Minus => compiler.emit_op(OpCode::Negate),
        _ => panic!("Unhandled unary token: {:?}", token_type),
    }
    Ok(())
}

fn binary(compiler: &mut Compiler) -> UnitResult {
    let token_type = compiler.parser.previous.token_type;
    let rule = get_rule(token_type);
    parse_precedence(compiler, rule.precedence + 1)?;
    match token_type {
        TokenType::BangEqual => compiler.emit_ops(OpCode::Equal, OpCode::Not),
        TokenType::EqualEqual => compiler.emit_op(OpCode::Equal),
        TokenType::Greater => compiler.emit_op(OpCode::Greater),
        TokenType::GreaterEqual => compiler.emit_ops(OpCode::Less, OpCode::Not),
        TokenType::Less => compiler.emit_op(OpCode::Less),
        TokenType::LessEqual => compiler.emit_ops(OpCode::Greater, OpCode::Not),
        TokenType::Plus => compiler.emit_op(OpCode::Add),
        TokenType::Minus => compiler.emit_op(OpCode::Subtract),
        TokenType::Star => compiler.emit_op(OpCode::Multiply),
        TokenType::Slash => compiler.emit_op(OpCode::Divide),
        _ => panic!("Unhandled binary token: {:?}", token_type),
    }
    Ok(())
}

fn build_error(message: &str, line: u32) -> BasicError {
    BasicError::new(&format!("Compilation error at line {line}: {message}"))
}
