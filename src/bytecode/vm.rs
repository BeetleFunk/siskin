use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;
use std::time::Instant;

use once_cell::sync::Lazy;

use crate::error::{BasicError, BasicResult};

use super::code::{self, Function, NativeFunction, OpCode, Value};
use super::compiler;

const DEBUG_TRACING: bool = false;

const FRAMES_MAX: usize = 256;

// TODO: make this thread local and avoid requirement on sync?
static EPOCH: Lazy<Instant> = Lazy::new(Instant::now);

struct State {
    locals: Vec<Value>,
    globals: HashMap<String, Value>,
    call_stack: Vec<CallFrame>,
}

impl State {
    pub fn new(root_frame: CallFrame) -> Self {
        State {
            locals: Vec::new(),
            globals: create_globals(),
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

fn create_globals() -> HashMap<String, Value> {
    let mut globals = HashMap::new();

    let library = [
        NativeFunction {
            arity: 0,
            func: native_clock,
            name: "clock".to_string(),
        },
        NativeFunction {
            arity: 1,
            func: native_sqrt,
            name: "sqrt".to_string(),
        },
    ];

    // add standard library native functions into the globals
    for func in library {
        globals.insert(func.name.clone(), Value::from(func));
    }
    globals
}

fn native_clock(_args: &[Value]) -> Value {
    // make sure epoch is initialized first (lazy init)
    let epoch = *EPOCH;
    let duration = Instant::now() - epoch;
    // lossy conversion to f64 here, shouldn't be an issue for a while though
    (duration.as_millis() as f64).into()
}

fn native_sqrt(args: &[Value]) -> Value {
    if args.len() != 1 {
        panic!("Native function invoked with incorrect arg count.");
    }

    if let Value::Number(value) = args[0] {
        value.sqrt().into()
    } else {
        // TODO: runtime errors from native functions
        panic!("Expected number argument in sqrt function.");
    }
}

struct CallFrame {
    ip: usize,
    locals_base: usize, // base index for function locals within the VM locals stack
    function: Rc<Function>,
}

pub fn interpret(source: &str, output: &mut dyn Write) -> BasicResult<()> {
    let root_func = compiler::compile(source)?;
    let mut vm_state = State::new(CallFrame {
        ip: 0,
        locals_base: 0,
        function: Rc::new(root_func),
    });
    let result = execute(&mut vm_state, output);

    if let Err(ref e) = result {
        writeln!(output, " --- {}", e.description).expect("Output writer should succeed.");
        print_stack_trace(&vm_state, output);
    }

    result
}

fn print_stack_trace(state: &State, output: &mut dyn Write) {
    for frame in state.call_stack.iter().rev() {
        let line = frame.function.chunk.line_numbers[frame.ip - 1];
        let name = if frame.function.name.is_empty() {
            "script"
        } else {
            &frame.function.name
        };
        writeln!(output, " ---   line {line} in {name}").expect("Output writer should succeed.");
    }
}

fn execute(state: &mut State, output: &mut dyn Write) -> BasicResult<()> {
    loop {
        if DEBUG_TRACING {
            print_stack(&state.locals);
            let frame = state.frame();
            code::disassemble_instruction(&frame.function.chunk, frame.ip);
        }

        let opcode: OpCode = read_byte(state).into();
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
                let value = read_constant(state);
                state.locals.push(value);
            }
            OpCode::Negate => {
                if let Value::Number(value) = state.locals.pop().unwrap() {
                    state.locals.push(Value::from(-value));
                } else {
                    return Err(build_error(
                        "Negation requires numeric operand",
                        last_line_number(state),
                    ));
                }
            }
            OpCode::Not => {
                if let Value::Bool(value) = state.locals.pop().unwrap() {
                    state.locals.push(Value::from(!value));
                } else {
                    return Err(build_error(
                        "Boolean inversion requires boolean operand",
                        last_line_number(state),
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
                        last_line_number(state),
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
                        last_line_number(state),
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
                        last_line_number(state),
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
                        last_line_number(state),
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
                        last_line_number(state),
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
                        last_line_number(state),
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
                if let Value::String(name) = read_constant(state) {
                    let value = state.locals.pop().unwrap();
                    state.globals.insert(name, value);
                } else {
                    panic!("DefineGlobal must have a string constant for the variable name.");
                }
            }
            OpCode::GetGlobal => {
                if let Value::String(name) = read_constant(state) {
                    if let Some(value) = state.globals.get(&name) {
                        state.locals.push(value.clone());
                    } else {
                        return Err(build_error(
                            &format!("Undefined variable {name}."),
                            last_line_number(state),
                        ));
                    }
                } else {
                    panic!("LoadGlobal must have a string constant for the variable name.");
                }
            }
            OpCode::SetGlobal => {
                if let Value::String(name) = read_constant(state) {
                    let entry = state.globals.entry(name);
                    if let std::collections::hash_map::Entry::Occupied(mut entry) = entry {
                        // avoid popping the value off the stack here, assignment result should be propagated
                        entry.insert(state.locals.last().unwrap().clone());
                    } else {
                        return Err(build_error(
                            &format!("Undefined variable {}.", entry.key()),
                            last_line_number(state),
                        ));
                    }
                } else {
                    panic!("LoadGlobal must have a string constant for the variable name.");
                }
            }
            OpCode::GetLocal => {
                let slot = read_byte(state);
                let local_index = state.frame().locals_base + (slot as usize);
                state.locals.push(state.locals[local_index].clone());
            }
            OpCode::SetLocal => {
                let slot = read_byte(state);
                let local_index = state.frame().locals_base + (slot as usize);
                state.locals[local_index] = state.locals.last().unwrap().clone();
            }
            OpCode::Jump => {
                let offset = read_short(state);
                state.frame_mut().ip += offset as usize;
            }
            OpCode::JumpIfFalse => {
                let offset = read_short(state);
                if let Value::Bool(condition_value) = state.locals.last().unwrap() {
                    if !condition_value {
                        state.frame_mut().ip += offset as usize;
                    }
                } else {
                    return Err(build_error(
                        "if statement requires boolean condition",
                        compute_line_number(state, -3),
                    ));
                }
            }
            OpCode::Loop => {
                let offset = read_short(state);
                state.frame_mut().ip -= offset as usize;
            }
            OpCode::Call => {
                let arg_count = read_byte(state);
                // cloning values should be cheap (functions use Rc)
                let callee: Value =
                    state.locals[state.locals.len() - 1 - (arg_count as usize)].clone();
                call_value(state, callee, arg_count)?;
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

    if DEBUG_TRACING {
        println!("Constant {index:04} = {value}");
    }

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
        return Err(build_error("Stack overflow.", last_line_number(state)));
    }

    match callee {
        Value::Function(function) => {
            if function.arity != arg_count {
                return Err(build_error(
                    &format!(
                        "Expected {} arguments but received {}.",
                        function.arity, arg_count
                    ),
                    last_line_number(state),
                ));
            }

            // this should point to the location of the callee on the stack, arguments will begin in slot 1 relative to this base pointer
            let locals_base = state.locals.len() - (arg_count as usize) - 1;

            state.call_stack.push(CallFrame {
                ip: 0,
                locals_base,
                function,
            })
        }
        Value::NativeFunction(native_function) => {
            if native_function.arity != arg_count {
                return Err(build_error(
                    &format!(
                        "Expected {} arguments but received {}.",
                        native_function.arity, arg_count
                    ),
                    last_line_number(state),
                ));
            }

            let args_begin = state.locals.len() - (arg_count as usize);
            let args = &state.locals[args_begin..state.locals.len()];
            let result = (native_function.func)(args);

            // make sure to pop the native function callable itself which is the entry before the first argument
            state.locals.drain((args_begin - 1)..state.locals.len());
            state.locals.push(result);
        }
        _ => {
            return Err(build_error(
                "Can only call functions and classes.",
                last_line_number(state),
            ))
        }
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
