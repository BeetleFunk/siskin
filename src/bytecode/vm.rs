use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

use crate::error::{BasicError, BasicResult};

use super::code::{self, Function, OpCode, Value};
use super::compiler;

static DEBUG_TRACING: bool = true;

static FRAMES_MAX: usize = 256;

struct State {
    //ip: usize,
    locals: Vec<Value>,
    globals: HashMap<String, Value>,
    call_stack: Vec<CallFrame>,
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
    function: Rc<Function>,
}

pub fn interpret(source: &str, output: &mut dyn Write) -> BasicResult<()> {
    let root_func = compiler::compile(source)?;
    let vm_state = State::new(CallFrame {
        ip: 0,
        locals_base: 0,
        function: Rc::new(root_func),
    });
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
                let result = state.locals.pop().unwrap();
                let previous_frame = state.call_stack.pop().unwrap();
                if state.call_stack.is_empty() {
                    // hit the end of the root function (script toplevel)
                    break;
                }

                // pop all locals left by the previous frame as well as the callee itself
                let start = previous_frame.locals_base;
                let end = state.locals.len();
                state.locals.drain(start..end); //.for_each(|value| println!("Dropping {value}"));

                // leave the result on top of the stack
                state.locals.push(result);
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
                let local_index = state.frame().locals_base + (slot as usize);
                state.locals.push(state.locals[local_index].clone());
            }
            OpCode::SetLocal => {
                let slot = read_byte(&mut state);
                let local_index = state.frame().locals_base + (slot as usize);
                state.locals[local_index] = state.locals.last().unwrap().clone();
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
            OpCode::Call => {
                let arg_count = read_byte(&mut state);
                // cloning values should be cheap (functions use Rc)
                let callee: Value = state.locals[state.locals.len() - 1 - (arg_count as usize)].clone();
                call_value(&mut state, callee, arg_count)?;
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

fn call_value(state: &mut State, callee: Value, arg_count: u8) -> BasicResult<()> {
    if state.call_stack.len() >= FRAMES_MAX {
        return Err(build_error("Stack overflow.", last_line_number(state)))
    }

    match callee {
        Value::Function(function) => {
            if function.arity != arg_count {
                return Err(build_error(&format!("Expected {} arguments but received {}.", function.arity, arg_count), last_line_number(state)))
            }

            // this should point to the location of the callee on the stack, arguments will begin in slot 1 relative to this base pointer
            let locals_base = state.locals.len() - (arg_count as usize) - 1;

            state.call_stack.push(CallFrame { ip: 0, locals_base, function })
        }
        _ => return Err(build_error("Can only call functions and classes.", last_line_number(state))),
    }
    Ok(())
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
