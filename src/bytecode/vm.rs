use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;
use std::time::Instant;

use once_cell::sync::Lazy;

use crate::error::{BasicError, BasicResult};

use super::code::{self, Class, Closure, Instance, NativeFunction, OpCode, Upvalue, Value};
use super::compiler;

const DEBUG_TRACING: bool = true;

const FRAMES_MAX: usize = 256;

// TODO: make this thread local and avoid requirement on sync?
static EPOCH: Lazy<Instant> = Lazy::new(Instant::now);

struct State {
    call_stack: Vec<CallFrame>,
    value_stack: Vec<Value>,
    globals: HashMap<String, Value>,
    open_upvalues: Vec<Rc<Upvalue>>,
}

impl State {
    pub fn new(root_frame: CallFrame) -> Self {
        State {
            value_stack: Vec::new(),
            globals: create_globals(),
            call_stack: vec![root_frame],
            open_upvalues: Vec::new(),
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
    closure: Rc<Closure>,
}

pub fn interpret(source: &str, output: &mut dyn Write) -> BasicResult<()> {
    let root_func = compiler::compile(source)?;
    let mut vm_state = State::new(CallFrame {
        ip: 0,
        locals_base: 0,
        closure: Rc::new(Closure {
            function: Rc::new(root_func),
            upvalues: Vec::new(),
        }),
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
        let line = frame.closure.function.chunk.line_numbers[frame.ip - 1];
        let name = if frame.closure.function.name.is_empty() {
            "script"
        } else {
            &frame.closure.function.name
        };
        writeln!(output, " ---   line {line} in {name}").expect("Output writer should succeed.");
    }
}

fn execute(state: &mut State, output: &mut dyn Write) -> BasicResult<()> {
    loop {
        if DEBUG_TRACING {
            print_stack(&state.value_stack);
            let frame = state.frame();
            code::disassemble_instruction(&frame.closure.function.chunk, frame.ip);
        }

        let opcode: OpCode = read_byte(state).into();
        match opcode {
            OpCode::Return => {
                let result = state.value_stack.pop().unwrap();
                let previous_frame = state.call_stack.pop().unwrap();
                if state.call_stack.is_empty() {
                    // hit the end of the root function (script toplevel)
                    break;
                } else {
                    // pop all locals left by the previous frame as well as the callee itself
                    pop_stack_and_close_upvalues(state, previous_frame.locals_base);
                    // leave the function result on top of the stack
                    state.value_stack.push(result);
                }
            }
            OpCode::Constant => {
                let value = read_constant(state);
                state.value_stack.push(value);
            }
            OpCode::Negate => {
                if let Value::Number(value) = state.value_stack.pop().unwrap() {
                    state.value_stack.push(Value::from(-value));
                } else {
                    return Err(build_error(
                        "Negation requires numeric operand",
                        last_line_number(state),
                    ));
                }
            }
            OpCode::Not => {
                if let Value::Bool(value) = state.value_stack.pop().unwrap() {
                    state.value_stack.push(Value::from(!value));
                } else {
                    return Err(build_error(
                        "Boolean inversion requires boolean operand",
                        last_line_number(state),
                    ));
                }
            }
            OpCode::Equal => {
                let b = state.value_stack.pop().unwrap();
                let a = state.value_stack.pop().unwrap();
                state.value_stack.push(Value::from(a == b));
            }
            OpCode::Greater => {
                let operands = pop_binary_operands(&mut state.value_stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.value_stack.push(Value::from(a > b));
                } else {
                    return Err(build_error(
                        "Comparison requires numeric operands",
                        last_line_number(state),
                    ));
                }
            }
            OpCode::Less => {
                let operands = pop_binary_operands(&mut state.value_stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.value_stack.push(Value::from(a < b));
                } else {
                    return Err(build_error(
                        "Comparison requires numeric operands",
                        last_line_number(state),
                    ));
                }
            }
            OpCode::Add => {
                let operands = pop_binary_operands(&mut state.value_stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.value_stack.push(Value::from(a + b));
                } else if let (Value::String(a), Value::String(b)) = operands {
                    state.value_stack.push(Value::from(a + &b));
                } else {
                    return Err(build_error(
                        "Addition requires either numeric or string operands",
                        last_line_number(state),
                    ));
                }
            }
            OpCode::Subtract => {
                let operands = pop_binary_operands(&mut state.value_stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.value_stack.push(Value::from(a - b));
                } else {
                    return Err(build_error(
                        "Subtraction requires numeric operands",
                        last_line_number(state),
                    ));
                }
            }
            OpCode::Multiply => {
                let operands = pop_binary_operands(&mut state.value_stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.value_stack.push(Value::from(a * b));
                } else {
                    return Err(build_error(
                        "Multiplication requires numeric operands",
                        last_line_number(state),
                    ));
                }
            }
            OpCode::Divide => {
                let operands = pop_binary_operands(&mut state.value_stack);
                if let (Value::Number(a), Value::Number(b)) = operands {
                    state.value_stack.push(Value::from(a / b));
                } else {
                    return Err(build_error(
                        "Division requires numeric operands",
                        last_line_number(state),
                    ));
                }
            }
            OpCode::Nil => state.value_stack.push(Value::Nil),
            OpCode::True => state.value_stack.push(Value::Bool(true)),
            OpCode::False => state.value_stack.push(Value::Bool(false)),
            OpCode::Print => writeln!(output, "{}", state.value_stack.pop().unwrap())
                .expect("Output writer should succeed."),
            OpCode::Pop => {
                state.value_stack.pop();
            }
            OpCode::DefineGlobal => {
                let name = read_string_constant(state);
                let value = state.value_stack.pop().unwrap();
                state.globals.insert(name, value);
            }
            OpCode::GetGlobal => {
                let name = read_string_constant(state);
                if let Some(value) = state.globals.get(&name) {
                    state.value_stack.push(value.clone());
                } else {
                    return Err(build_error(
                        &format!("Undefined variable {name}."),
                        last_line_number(state),
                    ));
                }
            }
            OpCode::SetGlobal => {
                let name = read_string_constant(state);
                let entry = state.globals.entry(name);
                if let std::collections::hash_map::Entry::Occupied(mut entry) = entry {
                    // avoid popping the value off the stack here, assignment result should be propagated
                    entry.insert(state.value_stack.last().unwrap().clone());
                } else {
                    return Err(build_error(
                        &format!("Undefined variable {}.", entry.key()),
                        last_line_number(state),
                    ));
                }
            }
            OpCode::GetLocal => {
                let slot = read_byte(state);
                let local_index = state.frame().locals_base + (slot as usize);
                state
                    .value_stack
                    .push(state.value_stack[local_index].clone());
            }
            OpCode::SetLocal => {
                let slot = read_byte(state);
                let local_index = state.frame().locals_base + (slot as usize);
                state.value_stack[local_index] = state.value_stack.last().unwrap().clone();
            }
            OpCode::Jump => {
                let offset = read_short(state);
                state.frame_mut().ip += offset as usize;
            }
            OpCode::JumpIfFalse => {
                let offset = read_short(state);
                if let Value::Bool(condition_value) = state.value_stack.last().unwrap() {
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
                    state.value_stack[state.value_stack.len() - 1 - (arg_count as usize)].clone();
                call_value(state, callee, arg_count)?;
            }
            OpCode::Closure => {
                let value = read_constant(state);
                if let Value::Function(function) = value {
                    let mut upvalues = Vec::new();
                    for _ in 0..function.upvalue_count {
                        let is_local = read_byte(state) != 0;
                        let slot_index = read_byte(state);
                        if is_local {
                            let stack_index = state.frame().locals_base + (slot_index as usize);
                            upvalues.push(create_upvalue(state, stack_index));
                        } else {
                            // copy the upvalue from the enclosing function (current frame on the top of the call stack)
                            let enclosing_upvalue =
                                &state.frame().closure.upvalues[slot_index as usize];
                            upvalues.push(enclosing_upvalue.clone())
                        }
                    }
                    state
                        .value_stack
                        .push(Value::from(Closure { function, upvalues }));
                } else {
                    panic!("Expected function constant for OpCode::Closure instruction.");
                }
            }
            OpCode::GetUpvalue => {
                let upvalue_slot = read_byte(state);
                let upvalue = &state.frame().closure.upvalues[upvalue_slot as usize];
                // determine whether the upvalue is closed (RefCell on the heap) or still open (index on the stack)
                let cloned_val = if let Some(value) = upvalue.closed.borrow().as_ref() {
                    value.clone()
                } else {
                    state.value_stack[upvalue.stack_index].clone()
                };
                state.value_stack.push(cloned_val);
            }
            OpCode::SetUpvalue => {
                let new_value = state.value_stack.last().unwrap().clone();
                let upvalue_slot = read_byte(state);
                let upvalue = &state.frame().closure.upvalues[upvalue_slot as usize];
                // determine whether the upvalue is closed (RefCell on the heap) or still open (index on the stack)
                if upvalue.closed.borrow().is_none() {
                    let stack_location = upvalue.stack_index;
                    state.value_stack[stack_location] = new_value;
                } else {
                    *upvalue.closed.borrow_mut() = Some(new_value);
                }
            }
            OpCode::CloseUpvalue => {
                let stack_index = state.value_stack.len() - 1;
                let value = state.value_stack.pop().unwrap();
                let upvalue_closed = close_upvalue(state, stack_index, value);
                // for sanity checking and troubleshooting
                if !upvalue_closed {
                    panic!("CloseUpvalue instruction did not find open upvalue")
                }
            }
            OpCode::Class => {
                let class_name = read_string_constant(state);
                state
                    .value_stack
                    .push(Value::from(Class { name: class_name }))
            }
            OpCode::GetProperty => {
                let property_name = read_string_constant(state);
                if let Value::Instance(instance) = state.value_stack.pop().unwrap() {
                    if let Some(value) = instance.fields.borrow().get(&property_name) {
                        state.value_stack.push(value.clone());
                    } else {
                        return Err(build_error(
                            &format!("Undefined property '{}'.", property_name),
                            last_line_number(state)
                        ));
                    }
                } else {
                    return Err(build_error(
                        "Property access only allowed for instances.",
                        last_line_number(state)
                    ));
                }
            }
            OpCode::SetProperty => {
                let property_name = read_string_constant(state);
                // the value to set will be at the top of the stack and the instance will be the next entry after that
                let property_value = state.value_stack.pop().unwrap();
                if let Value::Instance(instance) = state.value_stack.pop().unwrap() {
                    instance.fields.borrow_mut().insert(property_name, property_value.clone());
                    // leave the value on top of the stack (the result of an assignment expression can be used by another expression)
                    state.value_stack.push(property_value);
                } else {
                    return Err(build_error(
                        "Property access only allowed for instances.",
                        last_line_number(state)
                    ));
                }
            }
        }
    }

    Ok(())
}

fn read_byte(state: &mut State) -> u8 {
    let frame = state.frame_mut();
    let byte = frame.closure.function.chunk.code[frame.ip];
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
    let value = &state.frame().closure.function.chunk.values[index as usize];

    if DEBUG_TRACING {
        println!("Constant {index:04} = {value}");
    }

    // TODO: need to clone this?
    value.clone()
}

fn read_string_constant(state: &mut State) -> String {
    let value = read_constant(state);
    if let Value::String(string_value) = value {
        string_value
    } else {
        panic!("Expected string value in the constant table.");
    }
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
        Value::Closure(closure) => {
            if closure.function.arity != arg_count {
                return Err(build_error(
                    &format!(
                        "Expected {} arguments but received {}.",
                        closure.function.arity, arg_count
                    ),
                    last_line_number(state),
                ));
            }

            // this should point to the location of the callee on the stack, arguments will begin in slot 1 relative to this base pointer
            let locals_base = state.value_stack.len() - (arg_count as usize) - 1;

            state.call_stack.push(CallFrame {
                ip: 0,
                locals_base,
                closure,
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

            let args_begin = state.value_stack.len() - (arg_count as usize);
            let args = &state.value_stack[args_begin..state.value_stack.len()];
            let result = (native_function.func)(args);

            // make sure to pop the native function callable itself which is the entry before the first argument
            state
                .value_stack
                .drain((args_begin - 1)..state.value_stack.len());
            state.value_stack.push(result);
        }
        Value::Class(class) => {
            if arg_count != 0 {
                return Err(build_error(
                    "Constructor arguments not yet supported.",
                    last_line_number(state),
                ));
            }

            let instance = Value::from(Instance {
                class,
                fields: RefCell::new(HashMap::new()),
            });
            // replace the class value on the stack with the newly created instance
            let callee_location = state.value_stack.len() - (arg_count as usize) - 1;
            state.value_stack[callee_location] = instance;
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

fn create_upvalue(state: &mut State, stack_index: usize) -> Rc<Upvalue> {
    let upvalue = Rc::new(Upvalue {
        stack_index,
        closed: RefCell::new(None),
    });
    state.open_upvalues.push(upvalue.clone());
    upvalue
}

// returns true if matching upvalue was closed, false if not found
fn close_upvalue(state: &mut State, stack_index: usize, value: Value) -> bool {
    // TODO: will the upvalue to close always be at the end of the list?
    let position_rev = state
        .open_upvalues
        .iter()
        .rev()
        .position(|upvalue| upvalue.stack_index == stack_index);
    if let Some(position_rev) = position_rev {
        let position = (state.open_upvalues.len() - 1) - position_rev;
        let upvalue = state.open_upvalues.remove(position);
        if DEBUG_TRACING {
            let other_refs = Rc::strong_count(&upvalue) - 1;
            println!("Closing upvalue with {other_refs} remaining references.");
        }
        *upvalue.closed.borrow_mut() = Some(value);
        true
    } else {
        false
    }
}

// remove all entries at and above stack_index_begin, and close any corresponding upvalues
fn pop_stack_and_close_upvalues(state: &mut State, stack_index_begin: usize) {
    let end = state.value_stack.len();
    let popped_values_rev: Vec<Value> = state
        .value_stack
        .drain(stack_index_begin..end)
        .rev()
        .collect();
    let mut stack_index_rev = (stack_index_begin..end).rev();
    for value in popped_values_rev {
        // TODO: this can be optimized by stopping as soon as a lower stack index is reached in the upvalue reverse list search
        close_upvalue(state, stack_index_rev.next().unwrap(), value);
    }
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
    frame.closure.function.chunk.line_numbers[index as usize]
}

fn build_error(message: &str, line: u32) -> BasicError {
    BasicError::new(&format!("Execution error at line {line}: {message}"))
}
