use std::mem;
use std::result;

use crate::error::BasicError;
use crate::scanner::{Scanner, Token, TokenType};

use super::code::{Chunk, OpCode};

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
            previous: None
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
const PREC_ASSIGNMENT: u32 = 1;  // =
const PREC_OR: u32 = 2;          // or
const PREC_AND: u32 = 3;         // and
const PREC_EQUALITY: u32 = 4;    // == !=
const PREC_COMPARISON: u32 = 5;  // < > <= >=
const PREC_TERM: u32 = 6;        // + -
const PREC_FACTOR: u32 = 7;      // * /
const PREC_UNARY: u32 = 8;       // ! -
const PREC_CALL: u32 = 9;        // . ()

struct ParseRule {
    prefix: Option<fn(&mut Compiler)>,
    infix: Option<fn(&mut Compiler)>,
    precedence: u32,
}

const PARSE_TABLE: [(TokenType, ParseRule); 1] = [(TokenType::Number(0.0), ParseRule { prefix: Some(number), infix: None, precedence: PREC_NONE })];

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
        if compiler.parser.current.token_type != TokenType::Eof {
            panic!("Expect EOF token type as the final scanner token.")
        }
    }

    Ok(chunk)
}

struct Compiler {
    parser: Parser,
    chunk: Chunk,
}

impl Compiler {
    fn new(parser: Parser, chunk: Chunk) -> Compiler {
        Compiler {
            parser,
            chunk
        }
    }

    fn emit_byte(&mut self, byte: OpCode) {
        self.chunk.write_op(byte, self.parser.previous.as_ref().unwrap().line);
    }

    fn emit_bytes(&mut self, byte1: OpCode, byte2: u8) {
        self.chunk.write_op(byte1, self.parser.previous.as_ref().unwrap().line);
        self.chunk.write(byte2, self.parser.previous.as_ref().unwrap().line);
    }
}

fn number(compiler: &mut Compiler) {
    let token = compiler.parser.previous.as_ref().unwrap();
    if let TokenType::Number(value) = token.token_type {
        let address = compiler.chunk.add_constant(value.into());
        compiler.emit_bytes(OpCode::Constant, address);
    } else {
        panic!("Token at location is not a number.");
    }
}

fn build_error(message: &str, line: u32) -> BasicError {
    BasicError::new(&format!("Compilation error at line {line}: {message}"))
}
