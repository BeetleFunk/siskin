use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::io::Write;
use std::rc::Rc;

use crate::error::{BasicError, BasicResult};

use super::code::{self, CompiledConstant, CompiledFunction, OpCode};
use super::gc;
use super::stdlib;
use super::value::{
    BoundMethod, Class, Closure, HeapEntry, HeapRef, HeapValue, Instance, Upvalue, Value,
};

const DEBUG_TRACING: bool = false;

const FRAMES_MAX: usize = 256;

pub struct State {
    pub call_stack: Vec<CallFrame>,
    pub value_stack: Vec<Value>,
    pub value_heap: Vec<HeapEntry>,
    pub globals: HashMap<String, Value>,
    pub open_upvalues: Vec<Rc<Upvalue>>,
}

pub struct CallFrame {
    pub ip: usize,
    pub locals_base: usize, // base index for function locals within the VM value_stack
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
        setup_stdlib(&mut state);
        state
    }

    fn stack_pop(&mut self) -> Value {
        self.value_stack.pop().unwrap()
    }

    fn stack_peek(&self) -> &Value {
        self.value_stack.last().unwrap()
    }

    // offset from the end (stack top)
    fn stack_offset(&self, offset: usize) -> &Value {
        &self.value_stack[self.value_stack.len() - offset - 1]
    }

    fn get_heap_value(&self, loc: HeapRef) -> &HeapValue {
        &self.value_heap[loc.0].value
    }

    fn get_heap_value_mut(&mut self, loc: HeapRef) -> &mut HeapValue {
        &mut self.value_heap[loc.0].value
    }

    fn place_on_heap(&mut self, value: HeapValue) -> Value {
        self.value_heap.push(HeapEntry {
            value,
            marked: Cell::new(false),
        });
        Value::from(HeapRef(self.value_heap.len() - 1))
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

// add standard library native functions into the globals
fn setup_stdlib(state: &mut State) {
    let stdlib = stdlib::standard_library();
    for func in stdlib {
        let name = func.name.clone();
        let heap_entry = state.place_on_heap(HeapValue::from(func));
        state.globals.insert(name, heap_entry);
    }
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
                let result = state.stack_pop();
                pop_call_frame(state);
                if state.call_stack.is_empty() {
                    // hit the end of the root function (script toplevel)
                    break;
                } else {
                    // leave the function result on top of the stack
                    state.value_stack.push(result);
                }
            }
            OpCode::Constant => {
                let constant_value = Value::from(read_constant(state));
                state.value_stack.push(constant_value);
            }
            OpCode::Negate => {
                if let Value::Number(value) = state.stack_pop() {
                    state.value_stack.push(Value::from(-value));
                } else {
                    return Err(build_error(
                        "Negation requires numeric operand",
                        last_line_number(state),
                    ));
                }
            }
            OpCode::Not => {
                if let Value::Bool(value) = state.stack_pop() {
                    state.value_stack.push(Value::from(!value));
                } else {
                    return Err(build_error(
                        "Boolean inversion requires boolean operand",
                        last_line_number(state),
                    ));
                }
            }
            OpCode::Equal => {
                let b = state.stack_pop();
                let a = state.stack_pop();
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
                let printed = state
                    .value_stack
                    .pop()
                    .unwrap()
                    .to_string(&state.value_heap);
                writeln!(output, "{}", printed).expect("Output writer should succeed.");
            }
            OpCode::Pop => {
                state.value_stack.pop();
            }
            OpCode::DefineGlobal => {
                let name = read_string_constant(state);
                let value = state.stack_pop();
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
                // avoid popping the value off the stack here, assignment result should be propagated
                let value = state.stack_peek().clone();
                // get and modify the entry if it exists, return an error otherwise
                let entry = state.globals.entry(name);
                if let std::collections::hash_map::Entry::Occupied(mut entry) = entry {
                    entry.insert(value);
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
                state.value_stack[local_index] = state.stack_peek().clone();
            }
            OpCode::Jump => {
                let offset = read_short(state);
                state.frame_mut().ip += offset as usize;
            }
            OpCode::JumpIfFalse => {
                let offset = read_short(state);
                if let Value::Bool(condition_value) = state.stack_peek() {
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
                // cloning a callable value is cheap (heap entries are indexed by a single usize)
                let callee = state.stack_offset(arg_count as usize).clone();
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
                let value = state.place_on_heap(HeapValue::from(Closure { function, upvalues }));
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
                let new_value = state.stack_peek().clone();
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
                let value = state.stack_pop();
                // The compiler only emits this instruction when there's a known upvalue.
                // If no match is found then something is broken!
                if !close_upvalue(state, stack_index, value) {
                    panic!("CloseUpvalue instruction did not find open upvalue")
                }
            }
            OpCode::Class => {
                let name = read_string_constant(state);
                let class = state.place_on_heap(HeapValue::from(Class {
                    name,
                    methods: HashMap::new(),
                }));
                state.value_stack.push(class);
            }
            OpCode::GetProperty => {
                let property_name = read_string_constant(state);
                let instance = state.stack_pop();
                let (instance, instance_ref) =
                    try_load_instance(state, &instance).ok_or_else(|| {
                        build_error(
                            "Property access only allowed for instances.",
                            last_line_number(state),
                        )
                    })?;
                // fields take precedence over and can shadow class methods
                if let Some(field_value) = instance.fields.get(&property_name) {
                    state.value_stack.push(field_value.clone());
                } else {
                    // create the binding on demand if the property name matches a class method
                    let class = load_class_for_instance(state, instance);
                    let closure = class.methods.get(&property_name).ok_or_else(|| {
                        build_error(
                            &format!("Undefined property '{}'.", property_name),
                            last_line_number(state),
                        )
                    })?;
                    let bound_method = HeapValue::from(BoundMethod {
                        instance: instance_ref,
                        closure: *closure,
                    });
                    let bound_method = state.place_on_heap(bound_method);
                    state.value_stack.push(bound_method);
                }
            }
            OpCode::SetProperty => {
                let property_name = read_string_constant(state);
                // the value to set will be at the top of the stack and the instance will be the next entry after that
                let property_value = state.stack_pop();
                let instance = state.stack_pop();
                if let Some((instance, _)) = try_load_instance_mut(state, &instance) {
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
                let closure = state.stack_pop();
                let (_, closure) = try_load_closure(state, &closure).expect(
                    "Method instruction expects a closure type value to be on top of the stack.",
                );
                let class = state.stack_peek().clone();
                let (class, _) = try_load_class_mut(state, &class)
                    .expect("The target class must be on the value stack for method creation.");
                class.methods.insert(name, closure);
            }
            OpCode::Invoke => {
                let property_name = read_string_constant(state);
                let arg_count = read_byte(state);
                let instance = state.stack_offset(arg_count as usize);
                let (instance, _) = try_load_instance(state, instance).ok_or_else(|| {
                    build_error(
                        "Property access only allowed for instances.",
                        last_line_number(state),
                    )
                })?;
                // fields take precedence and can shadow class methods
                let callable = if let Some(field_value) = instance.fields.get(&property_name) {
                    // standard convention when invoking a callable value: stack slot zero (the method receiver) should be set to the callable itself
                    let receiver_stack_index = state.value_stack.len() - (arg_count as usize) - 1;
                    let callable = field_value.clone();
                    state.value_stack[receiver_stack_index] = callable.clone();
                    callable
                } else {
                    let class = load_class_for_instance(state, instance);
                    let closure = class.methods.get(&property_name).ok_or_else(|| {
                        build_error(
                            &format!("Undefined property '{}'.", property_name),
                            last_line_number(state),
                        )
                    })?;
                    // optimized calling convention for class methods only: the receiving instance is already in place at stack slot zero, just call the closure value
                    Value::HeapRef(*closure)
                };
                call_value(state, callable, arg_count)?;
            }
            OpCode::Inherit => {
                let subclass = state.stack_pop();
                // leave the superclass on the stack, this local slot is used for any references to "super"
                let (superclass, _) =
                    try_load_class(state, state.stack_peek()).ok_or_else(|| {
                        build_error("Superclass must be a class.", last_line_number(state))
                    })?;
                let base_methods = superclass.methods.clone();
                let (subclass, _) = try_load_class_mut(state, &subclass).expect(
                    "Inherit instruction expects a class type value to be on top of the stack.",
                );
                subclass.methods.extend(base_methods);
            }
            OpCode::GetSuper => {
                let method_name = read_string_constant(state);
                let superclass = state.stack_pop();
                let (superclass, _) = try_load_class(state, &superclass).expect(
                    "GetSuper instruction expects a class type value to be on top of the stack.",
                );
                let closure = *superclass.methods.get(&method_name).ok_or_else(|| {
                    build_error(
                        &format!("Method {} doesn't exist on the superclass.", method_name),
                        last_line_number(state),
                    )
                })?;
                let instance = state.stack_pop();
                let (_, instance) = try_load_instance(state, &instance).expect(
                    "GetSuper instruction expects an instance type value to be on the stack.",
                );
                let bound_method =
                    state.place_on_heap(HeapValue::from(BoundMethod { instance, closure }));
                state.value_stack.push(bound_method);
            }
            OpCode::SuperInvoke => {
                let method_name = read_string_constant(state);
                let arg_count = read_byte(state);
                let superclass = state.stack_pop();
                let (superclass, _) = try_load_class(state, &superclass).expect(
                    "SuperInvoke instruction expects a class type value to be on top of the stack.",
                );
                let closure = *superclass.methods.get(&method_name).ok_or_else(|| {
                    build_error(
                        &format!("Method {} doesn't exist on the superclass.", method_name),
                        last_line_number(state),
                    )
                })?;
                // it's possible to call the method like a closure in this case because
                // the receiver instance is already in place on the stack, no need to create a method binding
                call_value(state, Value::HeapRef(closure), arg_count)?;
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

    let callee_ref = *callee.as_heap_ref().ok_or_else(|| {
        build_error(
            "Can only call functions and classes.",
            last_line_number(state),
        )
    })?;

    // Can't use fn get_heap_value() here because the current implementation requires split borrow on State fields
    let callee = &state.value_heap[callee_ref.0].value;

    // this should point to the current location of the callee on the stack, regular arguments will begin in slot 1 relative to this base pointer
    let locals_base = state.value_stack.len() - (arg_count as usize) - 1;

    match callee {
        HeapValue::BoundMethod(method) => {
            let closure = load_closure_for_method(state, method);
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
                .drain(locals_base..state.value_stack.len());

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
                let closure = try_load_closure(state, &Value::HeapRef(*location))
                    .unwrap_or_else(|| panic!("Method entry on class {} points to value at HeapRef({}) that isn't a closure.", class.name, location))
                    .0
                    .clone();
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
                class: callee_ref,
                fields: HashMap::new(),
            });
            // replace the class value (callee) on the stack with the newly created instance
            state.value_stack[locals_base] = state.place_on_heap(instance);
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
    // TODO: can this can be optimized by stopping as soon as a lower stack index is encountered (i.e. is [open_upvalues.stack_index] monotonic?)
    for upvalue in open_upvalues_rev {
        if upvalue.stack_index == stack_index {
            return upvalue.clone();
        }
    }
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
            let value_as_string = value.to_string(&state.value_heap);
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

// pop the last call frame, remove stack locals for that frame, and close any upvalues referencing those locals
fn pop_call_frame(state: &mut State) {
    let frame = state.call_stack.pop().unwrap();
    let locals_range = frame.locals_base..state.value_stack.len();
    let popped_values_rev: Vec<_> = state
        .value_stack
        .drain(locals_range.clone())
        .rev()
        .collect();
    for (stack_index, value) in std::iter::zip(locals_range.rev(), popped_values_rev) {
        // TODO: can this can be optimized by stopping as soon as a lower stack index is reached in the upvalue reverse list search?
        close_upvalue(state, stack_index, value);
    }
}

fn print_stack(state: &State) {
    print!("          ");
    for entry in &state.value_stack {
        print!("[ {} ]", entry.to_string(&state.value_heap));
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

// attempts to load a Class using the provided Value::HeapRef
// returns None if the provided value is not a HeapRef or if the corresponding heap entry is not a Class
fn try_load_class<'a>(state: &'a State, value: &'_ Value) -> Option<(&'a Class, HeapRef)> {
    let heap_ref = *value.as_heap_ref()?;
    let class = state.get_heap_value(heap_ref).as_class()?;
    Some((class, heap_ref))
}

fn try_load_class_mut<'a>(
    state: &'a mut State,
    value: &'_ Value,
) -> Option<(&'a mut Class, HeapRef)> {
    let heap_ref = *value.as_heap_ref()?;
    let class = state.get_heap_value_mut(heap_ref).as_class_mut()?;
    Some((class, heap_ref))
}

fn try_load_instance<'a>(state: &'a State, value: &'_ Value) -> Option<(&'a Instance, HeapRef)> {
    let heap_ref = *value.as_heap_ref()?;
    let instance = state.get_heap_value(heap_ref).as_instance()?;
    Some((instance, heap_ref))
}

fn try_load_instance_mut<'a>(
    state: &'a mut State,
    value: &'_ Value,
) -> Option<(&'a mut Instance, HeapRef)> {
    let heap_ref = *value.as_heap_ref()?;
    let instance = state.get_heap_value_mut(heap_ref).as_instance_mut()?;
    Some((instance, heap_ref))
}

fn try_load_closure<'a>(state: &'a State, value: &'_ Value) -> Option<(&'a Rc<Closure>, HeapRef)> {
    let heap_ref = *value.as_heap_ref()?;
    let closure = state.get_heap_value(heap_ref).as_closure()?;
    Some((closure, heap_ref))
}

fn load_class_for_instance<'a>(state: &'a State, instance: &'_ Instance) -> &'a Class {
    if let HeapValue::Class(class) = state.get_heap_value(instance.class) {
        class
    } else {
        // an instance should always point to a valid class, anything else would indicate a bug around instance creation or VM corruption
        panic!("Error loading class type {} for instance: The value at heap location {} is not a class.", instance.debug_class_name, instance.class);
    }
}

fn load_closure_for_method<'a>(state: &'a State, method: &'_ BoundMethod) -> &'a Rc<Closure> {
    if let HeapValue::Closure(closure) = state.get_heap_value(method.closure) {
        closure
    } else {
        panic!("Error loading closure for BoundMethod attached to instance at HeapRef({}): The value at HeapRef({}) is not a closure.", method.closure, method.instance);
    }
}
