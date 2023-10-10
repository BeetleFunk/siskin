use std::collections::HashMap;
use std::io::Write;

use crate::error::{BasicError, BasicResult};

use super::code::{self, Chunk, OpCode, Value};
use super::compiler;

static DEBUG_TRACING: bool = true;

struct State {
    ip: usize,
    stack: Vec<Value>,
    globals: HashMap<String, Value>,
}

impl State {
    pub fn new() -> Self {
        State {
            ip: 0,
            stack: Vec::new(),
            globals: HashMap::new(),
        }
    }
}

pub fn interpret(source: &str, output: &mut dyn Write) -> BasicResult<()> {
    let chunk = compiler::compile(source)?;
    execute(&chunk, output)
}

fn execute(chunk: &Chunk, output: &mut dyn Write) -> BasicResult<()> {
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
                    return Err(build_error(
                        "Negation requires numeric operand",
                        chunk.line_numbers[state.ip - 1],
                    ));
                }
            }
            OpCode::Not => {
                if let Value::Bool(value) = state.stack.pop().unwrap() {
                    state.stack.push(Value::from(!value));
                } else {
                    return Err(build_error(
                        "Boolean inversion requires boolean operand",
                        chunk.line_numbers[state.ip - 1],
                    ));
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
                    return Err(build_error(
                        "Comparison requires numeric operands",
                        chunk.line_numbers[state.ip - 1],
                    ));
                }
            }
            OpCode::Less => {
                let operands = pop_binary_operands(&mut state.stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.stack.push(Value::from(a < b));
                } else {
                    return Err(build_error(
                        "Comparison requires numeric operands",
                        chunk.line_numbers[state.ip - 1],
                    ));
                }
            }
            OpCode::Add => {
                let operands = pop_binary_operands(&mut state.stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.stack.push(Value::from(a + b));
                } else if let (Value::String(a), Value::String(b)) = operands {
                    state.stack.push(Value::from(a + &b));
                } else {
                    return Err(build_error(
                        "Addition requires either numeric or string operands",
                        chunk.line_numbers[state.ip - 1],
                    ));
                }
            }
            OpCode::Subtract => {
                let operands = pop_binary_operands(&mut state.stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.stack.push(Value::from(a - b));
                } else {
                    return Err(build_error(
                        "Subtraction requires numeric operands",
                        chunk.line_numbers[state.ip - 1],
                    ));
                }
            }
            OpCode::Multiply => {
                let operands = pop_binary_operands(&mut state.stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.stack.push(Value::from(a * b));
                } else {
                    return Err(build_error(
                        "Multiplication requires numeric operands",
                        chunk.line_numbers[state.ip - 1],
                    ));
                }
            }
            OpCode::Divide => {
                let operands = pop_binary_operands(&mut state.stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.stack.push(Value::from(a / b));
                } else {
                    return Err(build_error(
                        "Division requires numeric operands",
                        chunk.line_numbers[state.ip - 1],
                    ));
                }
            }
            OpCode::Nil => state.stack.push(Value::Nil),
            OpCode::True => state.stack.push(Value::Bool(true)),
            OpCode::False => state.stack.push(Value::Bool(false)),
            OpCode::Print => writeln!(output, "{}", state.stack.pop().unwrap())
                .expect("Output writer should succeed."),
            OpCode::Pop => {
                state.stack.pop();
            }
            OpCode::DefineGlobal => {
                if let Value::String(name) = read_constant(&mut state, chunk) {
                    let value = state.stack.pop().unwrap();
                    state.globals.insert(name, value);
                } else {
                    panic!("DefineGlobal must have a string constant for the variable name.");
                }
            }
            OpCode::GetGlobal => {
                if let Value::String(name) = read_constant(&mut state, chunk) {
                    if let Some(value) = state.globals.get(&name) {
                        state.stack.push(value.clone());
                    } else {
                        return Err(build_error(
                            &format!("Undefined variable {name}."),
                            chunk.line_numbers[state.ip - 1],
                        ));
                    }
                } else {
                    panic!("LoadGlobal must have a string constant for the variable name.");
                }
            }
            OpCode::SetGlobal => {
                if let Value::String(name) = read_constant(&mut state, chunk) {
                    let entry = state.globals.entry(name);
                    if let std::collections::hash_map::Entry::Occupied(mut entry) = entry {
                        // avoid popping the value off the stack here, assignment result should be propagated
                        entry.insert(state.stack.last().unwrap().clone());
                    } else {
                        return Err(build_error(
                            &format!("Undefined variable {}.", entry.key()),
                            chunk.line_numbers[state.ip - 1],
                        ));
                    }
                } else {
                    panic!("LoadGlobal must have a string constant for the variable name.");
                }
            }
            OpCode::GetLocal => {
                let slot = read_byte(&mut state, chunk);
                state.stack.push(state.stack[slot as usize].clone());
            }
            OpCode::SetLocal => {
                let slot = read_byte(&mut state, chunk);
                state.stack[slot as usize] = state.stack.last().unwrap().clone();
            }
            OpCode::Jump => {
                let offset = read_short(&mut state, chunk);
                state.ip += offset as usize;
            }
            OpCode::JumpIfFalse => {
                let offset = read_short(&mut state, chunk);
                if let Value::Bool(condition_value) = state.stack.last().unwrap() {
                    if !condition_value {
                        state.ip += offset as usize;
                    }
                } else {
                    return Err(build_error(
                        "if statement requires boolean condition",
                        chunk.line_numbers[state.ip - 2],
                    ));
                }
            }
        }
    }

    Ok(())
}

fn read_byte(state: &mut State, chunk: &Chunk) -> u8 {
    let byte = chunk.code[state.ip];
    state.ip += 1;
    byte
}

fn read_short(state: &mut State, chunk: &Chunk) -> u16 {
    let high_byte = chunk.code[state.ip];
    let low_byte = chunk.code[state.ip + 1];
    state.ip += 2;
    ((high_byte as u16) << 8) + low_byte as u16
}

fn read_constant(state: &mut State, chunk: &Chunk) -> Value {
    let index = read_byte(state, chunk);

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
