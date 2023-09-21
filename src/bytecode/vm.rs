use std::result;

use crate::error::BasicError;

use super::code::{self, Chunk, OpCode, Value};
use super::compiler;

static DEBUG_TRACING: bool = true;

struct State {
    ip: usize,
    stack: Vec<Value>,
}

impl State {
    pub fn new() -> Self {
        State {
            ip: 0,
            stack: Vec::new(),
        }
    }
}

pub fn interpret(source: &str) -> result::Result<(), BasicError> {
    compiler::compile(source);
    Ok(())
}

fn execute(chunk: &Chunk) -> result::Result<(), BasicError> {
    let mut state = State::new();

    while state.ip < chunk.code.len() {
        if DEBUG_TRACING {
            print_stack(&state.stack);
            code::disassemble_instruction(chunk, state.ip);
        }

        let opcode: OpCode = chunk.code[state.ip].into();
        state.ip += 1;

        match opcode {
            OpCode::Return => {
                println!("{}", state.stack.pop().expect("Stack should have an entry for return."));
                break;
            }
            OpCode::Constant => {
                let value = read_constant(&mut state, chunk);
                state.stack.push(value);
            }
            OpCode::Negate => {
                if state.stack.last().unwrap().is_number() {
                    let original = extract_number(state.stack.pop().unwrap());
                    state.stack.push(Value::from(-original));
                } else {
                    return Err(build_error("Negation requires numeric argument", chunk.line_numbers[state.ip - 1]));
                }
            }
            OpCode::Add => {
                let b = state.stack.pop().unwrap();
                let a = state.stack.pop().unwrap();
                if a.is_number() && b.is_number() {
                    state.stack.push(Value::from(extract_number(a) + extract_number(b)));
                } else {
                    return Err(build_error("Addition expects numeric arguments", chunk.line_numbers[state.ip - 1]));
                }
            }
            OpCode::Subtract => {
                let b = state.stack.pop().unwrap();
                let a = state.stack.pop().unwrap();
                if a.is_number() && b.is_number() {
                    state.stack.push(Value::from(extract_number(a) - extract_number(b)));
                } else {
                    return Err(build_error("Subtraction expects numeric arguments", chunk.line_numbers[state.ip - 1]));
                }
            }
            OpCode::Multiply => {
                let b = state.stack.pop().unwrap();
                let a = state.stack.pop().unwrap();
                if a.is_number() && b.is_number() {
                    state.stack.push(Value::from(extract_number(a) * extract_number(b)));
                } else {
                    return Err(build_error("Multiplication expects numeric arguments", chunk.line_numbers[state.ip - 1]));
                }
            }
            OpCode::Divide => {
                let b = state.stack.pop().unwrap();
                let a = state.stack.pop().unwrap();
                if a.is_number() && b.is_number() {
                    state.stack.push(Value::from(extract_number(a) / extract_number(b)));
                } else {
                    return Err(build_error("Division expects numeric arguments", chunk.line_numbers[state.ip - 1]));
                }
            }
        }
    }

    Ok(())
}

fn read_constant(state: &mut State, chunk: &Chunk) -> Value {
    let index = chunk.code[state.ip];
    state.ip += 1;

    let value = &chunk.values[index as usize];

    println!("Constant {index:04} = {value}");

    // TODO: need to clone this?
    value.clone()
}

fn extract_number(value: Value) -> f64 {
    match value {
        Value::Number(result) => result,
        _ => panic!("Expected number value, received ({value}) instead.")
    }
}

fn print_stack(stack: &Vec<Value>) {
    print!("          ");
    for entry in stack {
      print!("[ {entry} ]");
    }
    println!();
}

fn build_error(message: &str, line: u32) -> BasicError {
    BasicError::new(&format!("Execution error at line {line}: {message}"))
}
