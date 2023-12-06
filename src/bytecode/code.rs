use core::panic;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

const TRACE_VALUE_DROP: bool = true;

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
}

const OP_TABLE: [OpCode; 36] = [
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
];

impl From<u8> for OpCode {
    fn from(val: u8) -> OpCode {
        OP_TABLE[val as usize]
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
    Class(Rc<Class>),
    Instance(Rc<Instance>),
    BoundMethod(Rc<BoundMethod>), // runtime-only representation of a class method bound to an instance
    Function(Rc<Function>),       // compile time representation of a function or method
    Closure(Rc<Closure>), // runtime-only representation of a function (may have captured variables)
    NativeFunction(Rc<NativeFunction>),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Bool(value) => value.to_string(),
                Self::Nil => "Nil".to_string(),
                Self::Number(value) => value.to_string(),
                // TODO: avoid cloning strings for these cases
                Self::String(value) => value.clone(),
                Self::Class(value) => value.name.clone(),
                Self::Instance(value) => value.class.name.clone() + " instance",
                Self::BoundMethod(value) => format!("{} bound to instance of {}", value.closure.function.name, value.instance.class.name),
                Self::Function(value) => value.name.clone(),
                Self::Closure(value) => value.function.name.clone(),
                Self::NativeFunction(value) => value.name.clone(),
            }
        )
    }
}

impl From<bool> for Value {
    fn from(boolean: bool) -> Value {
        Value::Bool(boolean)
    }
}

impl From<f64> for Value {
    fn from(number: f64) -> Value {
        Value::Number(number)
    }
}

impl From<String> for Value {
    fn from(string: String) -> Value {
        Value::String(string)
    }
}

impl From<Class> for Value {
    fn from(class: Class) -> Value {
        Value::Class(Rc::new(class))
    }
}

impl From<Instance> for Value {
    fn from(instance: Instance) -> Value {
        Value::Instance(Rc::new(instance))
    }
}

impl From<BoundMethod> for Value {
    fn from(method: BoundMethod) -> Value {
        Value::BoundMethod(Rc::new(method))
    }
}

impl From<Function> for Value {
    fn from(func: Function) -> Value {
        Value::Function(Rc::new(func))
    }
}

impl From<Closure> for Value {
    fn from(closure: Closure) -> Value {
        Value::Closure(Rc::new(closure))
    }
}

impl From<NativeFunction> for Value {
    fn from(func: NativeFunction) -> Value {
        Value::NativeFunction(Rc::new(func))
    }
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub struct Class {
    pub name: String,
    pub methods: RefCell<HashMap<String, Rc<Closure>>>,
}

#[derive(Debug, PartialEq)]
pub struct Instance {
    pub class: Rc<Class>,
    pub fields: RefCell<HashMap<String, Value>>,
}

#[derive(Debug, PartialEq)]
pub struct BoundMethod {
    pub instance: Rc<Instance>,
    pub closure: Rc<Closure>,
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub arity: u8,
    pub upvalue_count: u8,
    pub chunk: Chunk,
    pub name: String,
}

#[derive(Debug, PartialEq)]
pub struct Closure {
    pub function: Rc<Function>,
    pub upvalues: Vec<Rc<Upvalue>>,
}

#[derive(Debug, PartialEq)]
pub struct Upvalue {
    pub stack_index: usize, // the location in the value stack if this upvalue has not yet been closed
    pub closed: RefCell<Option<Value>>, // the closed value once it has been moved to the heap
}

pub struct NativeFunction {
    pub arity: u8,
    pub func: fn(arguments: &[Value]) -> Value,
    pub name: String,
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &Self) -> bool {
        self.arity == other.arity && self.name == other.name
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("NativeFunction")
            .field("arity", &self.arity)
            .field("func", &"NATIVE_FUNCTION")
            .field("name", &self.name)
            .finish()
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
        OpCode::Closure => closure(chunk, offset),
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
    let value = &chunk.values[index as usize];
    println!("{opcode_name} {index:04} = {value}");
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

fn closure(chunk: &Chunk, offset: usize) -> usize {
    let function_index = chunk.code[offset + 1];
    let value = &chunk.values[function_index as usize];

    println!("CLOSURE {function_index:04} = {value}");
    if let Value::Function(function) = value {
        for i in 0..function.upvalue_count {
            let upvalue_offset = offset + 2 + (2 * (i as usize));
            let is_local = chunk.code[upvalue_offset];
            let slot_index = chunk.code[upvalue_offset + 1];
            let capture_type = if is_local == 0 { "upvalue" } else { "local" };
            println!("  | ({upvalue_offset:04}) captured: {capture_type} => slot {slot_index}");
        }

        disassemble_chunk(&function.chunk, &function.name);
        println!("== end {} ==", &function.name);

        // variable width encoding
        2 + (2 * (function.upvalue_count as usize))
    } else {
        panic!("Unexpected value for closure")
    }
}

fn invoke_instruction(opcode_name: &str, chunk: &Chunk, offset: usize) -> usize {
    let constant_index = chunk.code[offset + 1];
    let method_name = &chunk.values[constant_index as usize];
    let arg_count = chunk.code[offset + 2];
    println!("{opcode_name} ({arg_count} args) {constant_index:04} = {method_name}");
    3
}

// Drop tracing stuff for garbage collected value types

impl Drop for Class {
    fn drop(&mut self) {
        if TRACE_VALUE_DROP {
            println!(
                "Dropping class {} with {} methods.",
                self.name,
                self.methods.borrow().len()
            );
        }
    }
}

impl Drop for Instance {
    fn drop(&mut self) {
        if TRACE_VALUE_DROP {
            println!(
                "Dropping instance of {} with {} fields",
                self.class.name,
                self.fields.borrow().len()
            );
        }
    }
}

impl Drop for Closure {
    fn drop(&mut self) {
        if TRACE_VALUE_DROP {
            println!(
                "Dropping closure for function {} with {} upvalues",
                self.function.name,
                self.upvalues.len()
            );
        }
    }
}
