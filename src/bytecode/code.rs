use core::panic;
use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpCode {
    Return,
    Constant,
    Negate,
    Add,
    Subtract,
    Multiply,
    Divide,
}

const OP_TABLE: [OpCode; 7] = [
    OpCode::Return,
    OpCode::Constant,
    OpCode::Negate,
    OpCode::Add,
    OpCode::Subtract,
    OpCode::Multiply,
    OpCode::Divide,
];

impl From<u8> for OpCode {
    fn from(val: u8) -> OpCode {
        OP_TABLE[val as usize]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
}

impl Value {
    pub fn is_number(&self) -> bool {
        matches!(self, Value::Number(_))
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Number(value) => value.to_string(),
            }
        )
    }
}

impl From<f64> for Value {
    fn from(number: f64) -> Value {
        Value::Number(number)
    }
}

pub struct Chunk {
    pub code: Vec<u8>,
    pub values: Vec<Value>,
    pub line_numbers: Vec<u32>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            values: Vec::new(),
            line_numbers: Vec::new(),
        }
    }

    pub fn add_constant(&mut self, value: Value) -> u8 {
        let index = self.values.len();
        if index > u8::MAX.into() {
            panic!("Exceeded maximum number of values in chunk")
        }
        self.values.push(value);
        index as u8
    }

    pub fn write_op(&mut self, instruction: OpCode, line: u32) {
        self.write(instruction as u8, line);
    }

    pub fn write(&mut self, byte: u8, line: u32) {
        self.code.push(byte);
        self.line_numbers.push(line);
    }
}

pub fn disassemble_chunk(chunk: &Chunk, name: &str) {
    println!("== {name} ==");

    let mut offset: usize = 0;
    while offset < chunk.code.len() {
        offset += disassemble_instruction(chunk, offset);
    }
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize) -> usize {
    print!("{offset:04} ");

    if offset > 0 && chunk.line_numbers[offset] == chunk.line_numbers[offset - 1] {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.line_numbers[offset]);
    }

    let opcode = OpCode::from(chunk.code[offset]);
    match opcode {
        OpCode::Return => simple_instruction("Return"),
        OpCode::Constant => constant_instruction(chunk, offset),
        OpCode::Negate => simple_instruction("Negate"),
        OpCode::Add => simple_instruction("Add"),
        OpCode::Subtract => simple_instruction("Subtract"),
        OpCode::Multiply => simple_instruction("Multiply"),
        OpCode::Divide => simple_instruction("Divide"),
    }
}

fn simple_instruction(name: &str) -> usize {
    println!("{name}");
    1
}

fn constant_instruction(chunk: &Chunk, offset: usize) -> usize {
    let index = chunk.code[offset + 1];
    let value = &chunk.values[index as usize];
    println!("Constant {index:04} = {value}");
    2
}
