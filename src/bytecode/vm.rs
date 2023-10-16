use std::collections::HashMap;
use std::io::Write;

use crate::error::{BasicError, BasicResult};

use super::code::{self, OpCode, Value, Function};
use super::compiler;

static DEBUG_TRACING: bool = true;

struct State {
    //ip: usize,
    locals: Vec<Value>,
    globals: HashMap<String, Value>,
    call_stack: Vec<CallFrame>
}

impl State {
    pub fn new(root_frame: CallFrame) -> Self {
        State {
            locals: Vec::new(),
            globals: HashMap::new(),
            call_stack: vec![root_frame],
        }
    }

    // the current call frame
    fn frame(&self) -> &CallFrame {
        self.call_stack.last().unwrap()
    }

    // the current call frame (mut)
    fn frame_mut(&mut self) -> &mut CallFrame {
        self.call_stack.last_mut().unwrap()
    }
}

struct CallFrame {
    ip: usize,
    locals_base: usize, // base index for function locals within the VM locals stack
    function: Function,
}

pub fn interpret(source: &str, output: &mut dyn Write) -> BasicResult<()> {
    let root_func = compiler::compile(source)?;
    let vm_state = State::new(CallFrame{ ip: 0, locals_base: 0, function: root_func });
    execute(vm_state, output)
}

fn execute(mut state: State, output: &mut dyn Write) -> BasicResult<()> {
    loop {
        if DEBUG_TRACING {
            print_stack(&state.locals);
            let frame = state.frame();
            code::disassemble_instruction(&frame.function.chunk, frame.ip);
        }

        let opcode: OpCode = read_byte(&mut state).into();
        match opcode {
            OpCode::Return => {
                if state.call_stack.len() == 1 {
                    break;
                }
            }
            OpCode::Constant => {
                let value = read_constant(&mut state);
                state.locals.push(value);
            }
            OpCode::Negate => {
                if let Value::Number(value) = state.locals.pop().unwrap() {
                    state.locals.push(Value::from(-value));
                } else {
                    return Err(build_error(
                        "Negation requires numeric operand",
                        last_line_number(&state),
                    ));
                }
            }
            OpCode::Not => {
                if let Value::Bool(value) = state.locals.pop().unwrap() {
                    state.locals.push(Value::from(!value));
                } else {
                    return Err(build_error(
                        "Boolean inversion requires boolean operand",
                        last_line_number(&state),
                    ));
                }
            }
            OpCode::Equal => {
                let b = state.locals.pop().unwrap();
                let a = state.locals.pop().unwrap();
                state.locals.push(Value::from(a == b));
            }
            OpCode::Greater => {
                let operands = pop_binary_operands(&mut state.locals);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.locals.push(Value::from(a > b));
                } else {
                    return Err(build_error(
                        "Comparison requires numeric operands",
                        last_line_number(&state),
                    ));
                }
            }
            OpCode::Less => {
                let operands = pop_binary_operands(&mut state.locals);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.locals.push(Value::from(a < b));
                } else {
                    return Err(build_error(
                        "Comparison requires numeric operands",
                        last_line_number(&state),
                    ));
                }
            }
            OpCode::Add => {
                let operands = pop_binary_operands(&mut state.locals);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.locals.push(Value::from(a + b));
                } else if let (Value::String(a), Value::String(b)) = operands {
                    state.locals.push(Value::from(a + &b));
                } else {
                    return Err(build_error(
                        "Addition requires either numeric or string operands",
                        last_line_number(&state),
                    ));
                }
            }
            OpCode::Subtract => {
                let operands = pop_binary_operands(&mut state.locals);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.locals.push(Value::from(a - b));
                } else {
                    return Err(build_error(
                        "Subtraction requires numeric operands",
                        last_line_number(&state),
                    ));
                }
            }
            OpCode::Multiply => {
                let operands = pop_binary_operands(&mut state.locals);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.locals.push(Value::from(a * b));
                } else {
                    return Err(build_error(
                        "Multiplication requires numeric operands",
                        last_line_number(&state),
                    ));
                }
            }
            OpCode::Divide => {
                let operands = pop_binary_operands(&mut state.locals);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.locals.push(Value::from(a / b));
                } else {
                    return Err(build_error(
                        "Division requires numeric operands",
                        last_line_number(&state),
                    ));
                }
            }
            OpCode::Nil => state.locals.push(Value::Nil),
            OpCode::True => state.locals.push(Value::Bool(true)),
            OpCode::False => state.locals.push(Value::Bool(false)),
            OpCode::Print => writeln!(output, "{}", state.locals.pop().unwrap())
                .expect("Output writer should succeed."),
            OpCode::Pop => {
                state.locals.pop();
            }
            OpCode::DefineGlobal => {
                if let Value::String(name) = read_constant(&mut state) {
                    let value = state.locals.pop().unwrap();
                    state.globals.insert(name, value);
                } else {
                    panic!("DefineGlobal must have a string constant for the variable name.");
                }
            }
            OpCode::GetGlobal => {
                if let Value::String(name) = read_constant(&mut state) {
                    if let Some(value) = state.globals.get(&name) {
                        state.locals.push(value.clone());
                    } else {
                        return Err(build_error(
                            &format!("Undefined variable {name}."),
                            last_line_number(&state),
                        ));
                    }
                } else {
                    panic!("LoadGlobal must have a string constant for the variable name.");
                }
            }
            OpCode::SetGlobal => {
                if let Value::String(name) = read_constant(&mut state) {
                    let entry = state.globals.entry(name);
                    if let std::collections::hash_map::Entry::Occupied(mut entry) = entry {
                        // avoid popping the value off the stack here, assignment result should be propagated
                        entry.insert(state.locals.last().unwrap().clone());
                    } else {
                        return Err(build_error(
                            &format!("Undefined variable {}.", entry.key()),
                            last_line_number(&state),
                        ));
                    }
                } else {
                    panic!("LoadGlobal must have a string constant for the variable name.");
                }
            }
            OpCode::GetLocal => {
                let slot = read_byte(&mut state);
                state.locals.push(state.locals[slot as usize].clone());
            }
            OpCode::SetLocal => {
                let slot = read_byte(&mut state);
                state.locals[slot as usize] = state.locals.last().unwrap().clone();
            }
            OpCode::Jump => {
                let offset = read_short(&mut state);
                state.frame_mut().ip += offset as usize;
            }
            OpCode::JumpIfFalse => {
                let offset = read_short(&mut state);
                if let Value::Bool(condition_value) = state.locals.last().unwrap() {
                    if !condition_value {
                        state.frame_mut().ip += offset as usize;
                    }
                } else {
                    return Err(build_error(
                        "if statement requires boolean condition",
                        compute_line_number(&state, -3),
                    ));
                }
            }
            OpCode::Loop => {
                let offset = read_short(&mut state);
                state.frame_mut().ip -= offset as usize;
            }
        }
    }

    Ok(())
}

fn read_byte(state: &mut State) -> u8 {
    let frame = state.frame_mut();
    let byte = frame.function.chunk.code[frame.ip];
    frame.ip += 1;
    byte
}

fn read_short(state: &mut State) -> u16 {
    let high_byte = read_byte(state);
    let low_byte = read_byte(state);
    ((high_byte as u16) << 8) + low_byte as u16
}

fn read_constant(state: &mut State) -> Value {
    let index = read_byte(state);
    let value = &state.frame().function.chunk.values[index as usize];

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

fn last_line_number(state: &State) -> u32 {
    compute_line_number(state, -1)
}

fn compute_line_number(state: &State, offset: isize) -> u32 {
    let frame = state.frame();
    let index = (frame.ip as isize) + offset;
    frame.function.chunk.line_numbers[index as usize]
}

fn build_error(message: &str, line: u32) -> BasicError {
    BasicError::new(&format!("Execution error at line {line}: {message}"))
}
