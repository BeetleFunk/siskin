use super::value::CompiledConstant;

// special identifier strings that may be needed by both compiler and VM
pub const TYPE_INITIALIZER_METHOD: &str = "init";
pub const NAME_FOR_SELF: &str = "this";
pub const NAME_FOR_SUPERCLASS: &str = "super";

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum OpCode {
    Return,
    Constant,
    Negate,
    Not,
    Equal,
    Greater,
    Less,
    Add,
    Subtract,
    Multiply,
    Divide,
    Nil,
    True,
    False,
    Print,
    Pop,
    DefineGlobal,
    GetGlobal,
    SetGlobal,
    GetLocal,
    SetLocal,
    Jump,
    JumpIfFalse,
    Loop,
    Call,
    Closure,
    GetUpvalue,
    SetUpvalue,
    CloseUpvalue,
    Class,
    GetProperty,
    SetProperty,
    Method,
    Invoke,
    Inherit,
    GetSuper,
    SuperInvoke,
}

const OP_TABLE: [OpCode; 37] = [
    OpCode::Return,
    OpCode::Constant,
    OpCode::Negate,
    OpCode::Not,
    OpCode::Equal,
    OpCode::Greater,
    OpCode::Less,
    OpCode::Add,
    OpCode::Subtract,
    OpCode::Multiply,
    OpCode::Divide,
    OpCode::Nil,
    OpCode::True,
    OpCode::False,
    OpCode::Print,
    OpCode::Pop,
    OpCode::DefineGlobal,
    OpCode::GetGlobal,
    OpCode::SetGlobal,
    OpCode::GetLocal,
    OpCode::SetLocal,
    OpCode::Jump,
    OpCode::JumpIfFalse,
    OpCode::Loop,
    OpCode::Call,
    OpCode::Closure,
    OpCode::GetUpvalue,
    OpCode::SetUpvalue,
    OpCode::CloseUpvalue,
    OpCode::Class,
    OpCode::GetProperty,
    OpCode::SetProperty,
    OpCode::Method,
    OpCode::Invoke,
    OpCode::Inherit,
    OpCode::GetSuper,
    OpCode::SuperInvoke,
];

impl From<u8> for OpCode {
    fn from(val: u8) -> OpCode {
        OP_TABLE[val as usize]
    }
}

#[derive(Debug, PartialEq)]
pub struct Chunk {
    pub code: Vec<u8>,
    pub constants: Vec<CompiledConstant>,
    pub line_numbers: Vec<u32>,
}

impl Chunk {
    pub fn new() -> Self {
        Chunk {
            code: Vec::new(),
            constants: Vec::new(),
            line_numbers: Vec::new(),
        }
    }

    pub fn add_constant(&mut self, value: CompiledConstant) -> u8 {
        let index = self.constants.len();
        if index > u8::MAX.into() {
            panic!("Exceeded maximum number of values in chunk")
        }
        self.constants.push(value);
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
        offset += disassemble_instruction(chunk, offset, true);
    }

    println!("== end {name} ==");
}

pub fn disassemble_instruction(chunk: &Chunk, offset: usize, expand_functions: bool) -> usize {
    print!("{offset:04} ");

    if offset > 0 && chunk.line_numbers[offset] == chunk.line_numbers[offset - 1] {
        print!("   | ");
    } else {
        print!("{:4} ", chunk.line_numbers[offset]);
    }

    let opcode = OpCode::from(chunk.code[offset]);
    match opcode {
        OpCode::Return => simple_instruction("Return"),
        OpCode::Constant => constant_instruction("Constant", chunk, offset),
        OpCode::Negate => simple_instruction("Negate"),
        OpCode::Not => simple_instruction("Not"),
        OpCode::Equal => simple_instruction("Equal"),
        OpCode::Greater => simple_instruction("Greater"),
        OpCode::Less => simple_instruction("Less"),
        OpCode::Add => simple_instruction("Add"),
        OpCode::Subtract => simple_instruction("Subtract"),
        OpCode::Multiply => simple_instruction("Multiply"),
        OpCode::Divide => simple_instruction("Divide"),
        OpCode::Nil => simple_instruction("Nil"),
        OpCode::True => simple_instruction("True"),
        OpCode::False => simple_instruction("False"),
        OpCode::Print => simple_instruction("Print"),
        OpCode::Pop => simple_instruction("Pop"),
        OpCode::DefineGlobal => constant_instruction("DefineGlobal", chunk, offset),
        OpCode::GetGlobal => constant_instruction("GetGlobal", chunk, offset),
        OpCode::SetGlobal => constant_instruction("SetGlobal", chunk, offset),
        OpCode::GetLocal => byte_instruction("GetLocal", chunk, offset),
        OpCode::SetLocal => byte_instruction("SetLocal", chunk, offset),
        OpCode::Jump => short_instruction("Jump", chunk, offset),
        OpCode::JumpIfFalse => short_instruction("JumpIfFalse", chunk, offset),
        OpCode::Loop => short_instruction("Loop", chunk, offset),
        OpCode::Call => byte_instruction("Call", chunk, offset),
        OpCode::Closure => closure(chunk, offset, expand_functions),
        OpCode::GetUpvalue => byte_instruction("GetUpvalue", chunk, offset),
        OpCode::SetUpvalue => byte_instruction("SetUpvalue", chunk, offset),
        OpCode::CloseUpvalue => simple_instruction("CloseUpvalue"),
        OpCode::Class => constant_instruction("Class", chunk, offset),
        OpCode::GetProperty => constant_instruction("GetProperty", chunk, offset),
        OpCode::SetProperty => constant_instruction("SetProperty", chunk, offset),
        OpCode::Method => constant_instruction("Method", chunk, offset),
        OpCode::Invoke => invoke_instruction("Invoke", chunk, offset),
        OpCode::Inherit => simple_instruction("Inherit"),
        OpCode::GetSuper => constant_instruction("GetSuper", chunk, offset),
        OpCode::SuperInvoke => invoke_instruction("SuperInvoke", chunk, offset)
    }
}

fn simple_instruction(name: &str) -> usize {
    println!("{name}");
    1
}

// instruction with a single byte of data
fn byte_instruction(opcode_name: &str, chunk: &Chunk, offset: usize) -> usize {
    let byte = chunk.code[offset + 1];
    println!("{opcode_name} {byte:04}");
    2
}

fn constant_instruction(opcode_name: &str, chunk: &Chunk, offset: usize) -> usize {
    let index = chunk.code[offset + 1];
    let value = &chunk.constants[index as usize];
    println!("{opcode_name} {index:04} = {value:?}");
    2
}

// instruction with two bytes of data
fn short_instruction(opcode_name: &str, chunk: &Chunk, offset: usize) -> usize {
    let high_byte = chunk.code[offset + 1];
    let low_byte = chunk.code[offset + 2];
    let short = ((high_byte as u16) << 8) + low_byte as u16;
    println!("{opcode_name} {short:06}");
    3
}

fn closure(chunk: &Chunk, offset: usize, expand_functions: bool) -> usize {
    let function_index = chunk.code[offset + 1];
    let value = &chunk.constants[function_index as usize];

    println!("CLOSURE {function_index:04} = {value:?}");
    if let CompiledConstant::Function(function) = value {
        for i in 0..function.upvalue_count {
            let upvalue_offset = offset + 2 + (2 * (i as usize));
            let is_local = chunk.code[upvalue_offset];
            let slot_index = chunk.code[upvalue_offset + 1];
            let capture_type = if is_local == 0 { "upvalue" } else { "local" };
            println!("  | ({upvalue_offset:04}) captured: {capture_type} => slot {slot_index}");
        }

        if expand_functions {
            disassemble_chunk(&function.chunk, &function.name);
        }

        // variable width encoding
        2 + (2 * (function.upvalue_count as usize))
    } else {
        panic!("Unexpected value for closure")
    }
}

fn invoke_instruction(opcode_name: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant_index = chunk.code[offset + 1];
    let method_name = &chunk.constants[constant_index as usize];
    let arg_count = chunk.code[offset + 2];
    println!("{opcode_name} ({arg_count} args) {constant_index:04} = {method_name:?}");
    3
}
