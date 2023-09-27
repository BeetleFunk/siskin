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
    let chunk = compiler::compile(source)?;
    execute(&chunk)
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
                if let Value::Number(value) = state.stack.pop().unwrap() {
                    state.stack.push(Value::from(-value));
                } else {
                    return Err(build_error("Negation requires numeric operand", chunk.line_numbers[state.ip - 1]));
                }
            }
            OpCode::Not => {
                if let Value::Bool(value) = state.stack.pop().unwrap() {
                    state.stack.push(Value::from(!value));
                } else {
                    return Err(build_error("Boolean inversion requires boolean operand", chunk.line_numbers[state.ip - 1]));
                }
            }
            OpCode::Equal => {
                let b = state.stack.pop().unwrap();
                let a = state.stack.pop().unwrap();
                state.stack.push(Value::from(a == b));
            }
            OpCode::Greater => {
                let operands = pop_binary_operands(&mut state.stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.stack.push(Value::from(a > b));
                } else {
                    return Err(build_error("Comparison requires numeric operands", chunk.line_numbers[state.ip - 1]));
                }
            }
            OpCode::Less => {
                let operands = pop_binary_operands(&mut state.stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.stack.push(Value::from(a < b));
                } else {
                    return Err(build_error("Comparison requires numeric operands", chunk.line_numbers[state.ip - 1]));
                }
            }
            OpCode::Add => {
                let operands = pop_binary_operands(&mut state.stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.stack.push(Value::from(a + b));
                } else if let (Value::String(a), Value::String(b)) = operands {
                    state.stack.push(Value::from(a + &b));
                } else {
                    return Err(build_error("Addition requires either numeric or string operands", chunk.line_numbers[state.ip - 1]));
                }
            }
            OpCode::Subtract => {
                let operands = pop_binary_operands(&mut state.stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.stack.push(Value::from(a - b));
                } else {
                    return Err(build_error("Subtraction requires numeric operands", chunk.line_numbers[state.ip - 1]));
                }
            }
            OpCode::Multiply => {
                let operands = pop_binary_operands(&mut state.stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.stack.push(Value::from(a * b));
                } else {
                    return Err(build_error("Multiplication requires numeric operands", chunk.line_numbers[state.ip - 1]));
                }
            }
            OpCode::Divide => {
                let operands = pop_binary_operands(&mut state.stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.stack.push(Value::from(a / b));
                } else {
                    return Err(build_error("Division requires numeric operands", chunk.line_numbers[state.ip - 1]));
                }
            }
            OpCode::Nil => state.stack.push(Value::Nil),
            OpCode::True => state.stack.push(Value::Bool(true)),
            OpCode::False => state.stack.push(Value::Bool(false)),
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

fn pop_binary_operands(stack: &mut Vec<Value>) -> (Value, Value) {
    let b = stack.pop().unwrap();
    let a = stack.pop().unwrap();
    (a, b)
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
