use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;
use std::thread;
use std::time::{Duration, Instant};

use once_cell::sync::Lazy;

use crate::error::{BasicError, BasicResult};

use super::code::{self, CompiledConstant, CompiledFunction, OpCode};
use super::gc;
use super::value::{
    BoundMethod, Class, Closure, HeapEntry, HeapRef, HeapValue, Instance, NativeFunction, Upvalue,
    Value,
};

const DEBUG_TRACING: bool = false;

const FRAMES_MAX: usize = 256;

// TODO: make this thread local and avoid requirement on sync?
static EPOCH: Lazy<Instant> = Lazy::new(Instant::now);

pub struct State {
    pub call_stack: Vec<CallFrame>,
    pub value_stack: Vec<Value>,
    pub value_heap: Vec<HeapEntry>,
    pub globals: HashMap<String, Value>,
    pub open_upvalues: Vec<Rc<Upvalue>>,
}

pub struct CallFrame {
    pub ip: usize,
    pub locals_base: usize, // base index for function locals within the VM locals stack
    pub closure: Rc<Closure>,
}

impl State {
    pub fn new(root_frame: CallFrame) -> Self {
        let mut state = State {
            call_stack: vec![root_frame],
            value_stack: Vec::new(),
            value_heap: Vec::new(),
            globals: HashMap::new(),
            open_upvalues: Vec::new(),
        };
        setup_standard_library(&mut state);
        state
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

fn setup_standard_library(state: &mut State) {
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
        NativeFunction {
            arity: 1,
            func: native_to_string,
            name: "toString".to_string(),
        },
        NativeFunction {
            arity: 1,
            func: native_sleep,
            name: "sleep".to_string(),
        },
        NativeFunction {
            arity: 0,
            func: native_heap_check,
            name: "getNumHeapEntries".to_string(),
        },
    ];

    // add standard library native functions into the globals
    for func in library {
        let name = func.name.clone();
        let heap_entry = place_on_heap(state, HeapValue::from(func));
        state.globals.insert(name, heap_entry);
    }
}

fn native_clock(_heap: &[HeapEntry], _args: &[Value]) -> BasicResult<Value> {
    // make sure epoch is initialized first (lazy init)
    let epoch = *EPOCH;
    let duration = Instant::now() - epoch;
    // lossy conversion to f64 here, shouldn't be an issue for a while though
    Ok((duration.as_millis() as f64).into())
}

fn native_sqrt(_heap: &[HeapEntry], args: &[Value]) -> BasicResult<Value> {
    if let Value::Number(value) = args[0] {
        Ok(Value::from(value.sqrt()))
    } else {
        Err(BasicError::new(
            "Expected number argument for sqrt function.",
        ))
    }
}

fn native_to_string(heap: &[HeapEntry], args: &[Value]) -> BasicResult<Value> {
    Ok(Value::from(value_to_string(heap, &args[0])))
}

fn native_sleep(_heap: &[HeapEntry], args: &[Value]) -> BasicResult<Value> {
    if let Value::Number(value) = args[0] {
        if value < 0.0 {
            return Err(BasicError::new(
                "Expected positive number argument for sleep function.",
            ));
        }
        let duration = Duration::from_millis(value as u64);
        thread::sleep(duration);
        Ok(Value::Nil)
    } else {
        Err(BasicError::new(
            "Expected number argument for sleep function.",
        ))
    }
}

fn native_heap_check(heap: &[HeapEntry], _args: &[Value]) -> BasicResult<Value> {
    Ok(Value::Number(heap.len() as f64))
}

pub fn interpret(compiled: CompiledFunction, output: &mut dyn Write) -> BasicResult<()> {
    let mut vm_state = State::new(CallFrame {
        ip: 0,
        locals_base: 0,
        closure: Rc::new(Closure {
            function: Rc::new(compiled),
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
            print_stack(state);
            let frame = state.frame();
            code::disassemble_instruction(&frame.closure.function.chunk, frame.ip, false);
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
                let constant_value = Value::from(read_constant(state));
                state.value_stack.push(constant_value);
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
            OpCode::Print => {
                let printed = value_to_string(&state.value_heap, &state.value_stack.pop().unwrap());
                writeln!(output, "{}", printed).expect("Output writer should succeed.");
            }
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
                // cloning values should be cheap (heap entries are just references)
                let callee =
                    state.value_stack[state.value_stack.len() - 1 - (arg_count as usize)].clone();
                call_value(state, callee, arg_count)?;
            }
            OpCode::Closure => {
                let function = read_function_constant(state);
                let mut upvalues = Vec::new();
                for _ in 0..function.upvalue_count {
                    let is_local = read_byte(state) != 0;
                    let slot_index = read_byte(state);
                    if is_local {
                        let stack_index = state.frame().locals_base + (slot_index as usize);
                        upvalues.push(capture_upvalue(state, stack_index));
                    } else {
                        // copy the upvalue from the enclosing function (current frame on the top of the call stack)
                        let enclosing_upvalue =
                            &state.frame().closure.upvalues[slot_index as usize];
                        upvalues.push(enclosing_upvalue.clone())
                    }
                }
                let value = place_on_heap(state, HeapValue::from(Closure { function, upvalues }));
                state.value_stack.push(value);
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
                let name = read_string_constant(state);
                let class = place_on_heap(
                    state,
                    HeapValue::from(Class {
                        name,
                        methods: HashMap::new(),
                    }),
                );
                state.value_stack.push(class);
            }
            OpCode::GetProperty => {
                let property_name = read_string_constant(state);
                // fields take precedence over and can shadow class methods
                if let Some((location, instance)) = stack_pop_instance(state) {
                    if let Some(field_value) = instance.fields.get(&property_name) {
                        let value_copy = field_value.clone();
                        state.value_stack.push(value_copy);
                    } else {
                        let class = instance.class;
                        let class = get_class(state, class);
                        if let Some(closure) = class.methods.get(&property_name) {
                            let bound_method = HeapValue::from(BoundMethod {
                                instance: location,
                                closure: *closure,
                            });
                            let bound_method = place_on_heap(state, bound_method);
                            state.value_stack.push(bound_method);
                        } else {
                            return Err(build_error(
                                &format!("Undefined property '{}'.", property_name),
                                last_line_number(state),
                            ));
                        }
                    }
                } else {
                    return Err(build_error(
                        "Property access only allowed for instances.",
                        last_line_number(state),
                    ));
                }
            }
            OpCode::SetProperty => {
                let property_name = read_string_constant(state);
                // the value to set will be at the top of the stack and the instance will be the next entry after that
                let property_value = state.value_stack.pop().unwrap();
                if let Some((_, instance)) = stack_pop_instance_mut(state) {
                    instance
                        .fields
                        .insert(property_name, property_value.clone());
                    // leave the value on top of the stack (the result of an assignment expression can be used by another expression)
                    state.value_stack.push(property_value);
                } else {
                    return Err(build_error(
                        "Property access only allowed for instances.",
                        last_line_number(state),
                    ));
                }
            }
            OpCode::Method => {
                let name = read_string_constant(state);
                if let Some((method, _)) = stack_pop_closure(state) {
                    if let Some((_, class)) = stack_peek_class_mut(state) {
                        class.methods.insert(name, method);
                    } else {
                        panic!("The target class must be on the value stack for method creation.");
                    }
                } else {
                    panic!("Method instruction expects a closure type value to be on top of the stack.");
                };
            }
            OpCode::Invoke => {
                let property_name = read_string_constant(state);
                let arg_count = read_byte(state);
                let receiver = stack_peek_reference(state, arg_count as usize);

                if let Some((_, HeapValue::Instance(instance))) = receiver {
                    // fields take precedence and can shadow methods
                    let closure = if instance.fields.contains_key(&property_name) {
                        let field_value = instance.fields.get(&property_name).unwrap().clone();
                        // standard calling convention when invoking the value on a field: stack slot zero (the method receiver) should be the closure itself
                        let receiver_stack_index =
                            state.value_stack.len() - (arg_count as usize) - 1;
                        state.value_stack[receiver_stack_index] = field_value.clone();
                        field_value
                    } else {
                        let class = instance.class;
                        let class = get_class(state, class);
                        if let Some(closure) = class.methods.get(&property_name) {
                            // it's possible to call the method like a closure in this case because the receiver instance is already in place on the stack, no need to create a method binding
                            Value::HeapRef(*closure)
                        } else {
                            return Err(build_error(
                                &format!("Undefined property '{}'.", property_name),
                                last_line_number(state),
                            ));
                        }
                    };
                    call_value(state, closure, arg_count)?;
                } else {
                    return Err(build_error(
                        "Property access only allowed for instances.",
                        last_line_number(state),
                    ));
                }
            }
            OpCode::Inherit => {
                let (subclass, _) = stack_pop_reference(state).expect(
                    "Inherit instruction expects a class type value to be on top of the stack.",
                );
                // leave the superclass on the stack, the compiler uses this local slot for any references to "super"
                if let Some((_, superclass)) = stack_peek_class(state) {
                    let base_methods = superclass.methods.clone();
                    if let HeapValue::Class(subclass) = &mut state.value_heap[subclass.index].value
                    {
                        subclass.methods.extend(base_methods)
                    } else {
                        panic!("Inherit instruction expects a class type value to be on top of the stack.");
                    }
                } else {
                    return Err(build_error(
                        "Superclass must be a class.",
                        last_line_number(state),
                    ));
                }
            }
            OpCode::GetSuper => {
                let method_name = read_string_constant(state);
                let (_, superclass) = stack_pop_class(state).expect(
                    "GetSuper instruction expects a class type value to be on top of the stack.",
                );
                let closure = if let Some(closure) = superclass.methods.get(&method_name) {
                    *closure
                } else {
                    return Err(build_error(
                        &format!("Method {} doesn't exist on the superclass.", method_name),
                        last_line_number(state),
                    ));
                };
                let (instance, _) = stack_pop_instance(state).expect(
                    "GetSuper instruction expects an instance type value to be on the stack.",
                );
                let bound_method =
                    place_on_heap(state, HeapValue::from(BoundMethod { instance, closure }));
                state.value_stack.push(bound_method);
            }
            OpCode::SuperInvoke => {
                let method_name = read_string_constant(state);
                let arg_count = read_byte(state);
                let (_, superclass) = stack_pop_class(state).expect(
                    "SuperInvoke instruction expects a class type value to be on top of the stack.",
                );
                if let Some(closure_ref) = superclass.methods.get(&method_name) {
                    // it's possible to call the method like a closure in this case because the receiver instance is already in place on the stack, no need to create a method binding
                    let callee = Value::HeapRef(*closure_ref);
                    call_value(state, callee, arg_count)?;
                } else {
                    return Err(build_error(
                        &format!("Method {} doesn't exist on the superclass.", method_name),
                        last_line_number(state),
                    ));
                };
            }
        }

        // TODO: determine proper time to run this
        gc::collect_garbage(state);
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

fn read_constant(state: &mut State) -> &CompiledConstant {
    let index = read_byte(state);
    &state.frame().closure.function.chunk.constants[index as usize]
}

fn read_string_constant(state: &mut State) -> String {
    let constant = read_constant(state);
    if let CompiledConstant::String(string_value) = constant {
        string_value.to_owned()
    } else {
        panic!("Expected string type value in the constant table.");
    }
}

fn read_function_constant(state: &mut State) -> Rc<CompiledFunction> {
    let constant = read_constant(state);
    if let CompiledConstant::Function(function) = constant {
        function.clone()
    } else {
        panic!("Expected function type value in the constant table.");
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

    let heap_location = if let Value::HeapRef(heap_location) = callee {
        heap_location
    } else {
        return Err(build_error(
            "Can only call functions and classes.",
            last_line_number(state),
        ));
    };

    let callee = &state.value_heap[heap_location.index].value;

    // this should point to the current location of the callee on the stack, regular arguments will begin in slot 1 relative to this base pointer
    let locals_base = state.value_stack.len() - (arg_count as usize) - 1;

    match callee {
        HeapValue::BoundMethod(method) => {
            let closure = get_closure(state, method.closure);
            if closure.function.arity != arg_count {
                return Err(build_error(
                    &format!(
                        "Expected {} arguments but received {}.",
                        closure.function.arity, arg_count
                    ),
                    last_line_number(state),
                ));
            }
            state.call_stack.push(CallFrame {
                ip: 0,
                locals_base,
                closure: closure.clone(),
            });
            // rewrite local slot zero with the value of the bound method instance, used when referencing 'this' in function body
            state.value_stack[locals_base] = Value::HeapRef(method.instance);
        }
        HeapValue::Closure(closure) => {
            if closure.function.arity != arg_count {
                return Err(build_error(
                    &format!(
                        "Expected {} arguments but received {}.",
                        closure.function.arity, arg_count
                    ),
                    last_line_number(state),
                ));
            }
            state.call_stack.push(CallFrame {
                ip: 0,
                locals_base,
                closure: closure.clone(),
            })
        }
        HeapValue::NativeFunction(native_function) => {
            if native_function.arity != arg_count {
                return Err(build_error(
                    &format!(
                        "Expected {} arguments but received {}.",
                        native_function.arity, arg_count
                    ),
                    last_line_number(state),
                ));
            }
            let args_begin = locals_base + 1;
            let args = &state.value_stack[args_begin..state.value_stack.len()];
            // run the native function directly
            let result = (native_function.func)(&state.value_heap, args);
            // pop arguments and make sure to pop the native function callable itself which is the entry before the first argument
            state
                .value_stack
                .drain((locals_base)..state.value_stack.len());

            if let Err(e) = result {
                return Err(build_error(
                    &format!("Native function runtime error - {}", e.description),
                    last_line_number(state),
                ));
            } else {
                state.value_stack.push(result.unwrap());
            }
        }
        HeapValue::Class(class) => {
            // automatically run a class's type initializer (init() method) if it exists
            if let Some(location) = class.methods.get(code::TYPE_INITIALIZER_METHOD) {
                let closure = get_closure(state, *location).clone();
                if closure.function.arity != arg_count {
                    return Err(build_error(
                        &format!(
                            "Expected {} arguments to the type initializer for class {} but received {}.",
                            closure.function.arity, class.name, arg_count
                        ),
                        last_line_number(state),
                    ));
                }
                state.call_stack.push(CallFrame {
                    ip: 0,
                    locals_base,
                    closure,
                })
            } else if arg_count != 0 {
                return Err(build_error(
                        &format!("Unexpected arguments to initializer for class {} that has no init() method.", class.name),
                        last_line_number(state),
                    ));
            }
            let instance = HeapValue::from(Instance {
                debug_class_name: class.name.clone(),
                class: heap_location,
                fields: HashMap::new(),
            });
            // replace the class value (callee) on the stack with the newly created instance
            state.value_stack[locals_base] = place_on_heap(state, instance);
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

fn capture_upvalue(state: &mut State, stack_index: usize) -> Rc<Upvalue> {
    let open_upvalues_rev = state.open_upvalues.iter().rev();
    for upvalue in open_upvalues_rev {
        if upvalue.stack_index == stack_index {
            println!("Found existing upvalue for stack slot {stack_index}.");
            return upvalue.clone();
        }
    }

    let upvalue = Rc::new(Upvalue {
        stack_index,
        closed: RefCell::new(None),
    });
    state.open_upvalues.push(upvalue.clone());
    println!("Creating upvalue for stack slot {stack_index}.");
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
            let value_as_string = value_to_string(&state.value_heap, &value);
            println!(
                "Closing upvalue ({}) for stack slot {} with {} remaining references.",
                value_as_string, upvalue.stack_index, other_refs
            );
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
        // TODO: can this can be optimized by stopping as soon as a lower stack index is reached in the upvalue reverse list search?
        close_upvalue(state, stack_index_rev.next().unwrap(), value);
    }
}

fn print_stack(state: &State) {
    print!("          ");
    for entry in &state.value_stack {
        print!("[ {} ]", value_to_string(&state.value_heap, entry));
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

fn place_on_heap(state: &mut State, value: HeapValue) -> Value {
    state.value_heap.push(HeapEntry {
        value,
        marked: Cell::new(false),
    });
    Value::from(HeapRef {
        index: state.value_heap.len() - 1,
    })
}

fn stack_peek_reference(state: &State, offset: usize) -> Option<(HeapRef, &HeapValue)> {
    let stack_entry = &state.value_stack[state.value_stack.len() - offset - 1];
    if let Value::HeapRef(location) = stack_entry {
        Some((*location, &state.value_heap[location.index].value))
    } else {
        None
    }
}

fn stack_peek_reference_mut(state: &mut State) -> Option<(HeapRef, &mut HeapValue)> {
    let top = state.value_stack.last().unwrap();
    if let Value::HeapRef(location) = top {
        Some((*location, &mut state.value_heap[location.index].value))
    } else {
        None
    }
}

fn stack_pop_reference(state: &mut State) -> Option<(HeapRef, &HeapValue)> {
    let popped = state.value_stack.pop().unwrap();
    if let Value::HeapRef(location) = popped {
        Some((location, &state.value_heap[location.index].value))
    } else {
        None
    }
}

fn stack_pop_reference_mut(state: &mut State) -> Option<(HeapRef, &mut HeapValue)> {
    let popped = state.value_stack.pop().unwrap();
    if let Value::HeapRef(location) = popped {
        Some((location, &mut state.value_heap[location.index].value))
    } else {
        None
    }
}

fn stack_pop_closure(state: &mut State) -> Option<(HeapRef, &Closure)> {
    let value = stack_pop_reference(state);
    if let Some((location, HeapValue::Closure(closure))) = value {
        Some((location, closure))
    } else {
        None
    }
}

fn stack_pop_instance(state: &mut State) -> Option<(HeapRef, &Instance)> {
    let value = stack_pop_reference(state);
    if let Some((location, HeapValue::Instance(instance))) = value {
        Some((location, instance))
    } else {
        None
    }
}

fn stack_pop_instance_mut(state: &mut State) -> Option<(HeapRef, &mut Instance)> {
    let value = stack_pop_reference_mut(state);
    if let Some((location, HeapValue::Instance(instance))) = value {
        Some((location, instance))
    } else {
        None
    }
}

fn stack_pop_class(state: &mut State) -> Option<(HeapRef, &Class)> {
    let value = stack_pop_reference(state);
    if let Some((location, HeapValue::Class(class))) = value {
        Some((location, class))
    } else {
        None
    }
}

fn stack_peek_class(state: &mut State) -> Option<(HeapRef, &Class)> {
    let value = stack_peek_reference(state, 0);
    if let Some((location, HeapValue::Class(class))) = value {
        Some((location, class))
    } else {
        None
    }
}

fn stack_peek_class_mut(state: &mut State) -> Option<(HeapRef, &mut Class)> {
    let value = stack_peek_reference_mut(state);
    if let Some((location, HeapValue::Class(class))) = value {
        Some((location, class))
    } else {
        None
    }
}

fn get_closure(state: &State, location: HeapRef) -> &Rc<Closure> {
    if let HeapValue::Closure(closure) = &state.value_heap[location.index].value {
        closure
    } else {
        panic!("Expected closure at heap index {}", location.index);
    }
}

fn get_class(state: &State, location: HeapRef) -> &Class {
    if let HeapValue::Class(class) = &state.value_heap[location.index].value {
        class
    } else {
        panic!("Expected class at heap index {}", location.index);
    }
}

fn value_to_string(heap: &[HeapEntry], value: &Value) -> String {
    match value {
        Value::Bool(value) => value.to_string(),
        Value::Nil => "Nil".to_string(),
        Value::Number(value) => value.to_string(),
        Value::String(value) => value.clone(),
        Value::HeapRef(location) => {
            let heap_entry = &heap[location.index];
            heap_entry.value.to_string()
        }
    }
}
