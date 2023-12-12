use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::error::BasicResult;

use super::code::Chunk;

const TRACE_VALUE_DROP: bool = true;

#[derive(Debug, PartialEq)]
pub enum CompiledConstant {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
    Function(Rc<CompiledFunction>),
}

#[derive(Debug, PartialEq)]
pub struct CompiledFunction {
    pub arity: u8,
    pub upvalue_count: u8,
    pub chunk: Chunk,
    pub name: String,
}

impl From<bool> for CompiledConstant {
    fn from(boolean: bool) -> CompiledConstant {
        CompiledConstant::Bool(boolean)
    }
}

impl From<f64> for CompiledConstant {
    fn from(number: f64) -> CompiledConstant {
        CompiledConstant::Number(number)
    }
}

impl From<String> for CompiledConstant {
    fn from(string: String) -> CompiledConstant {
        CompiledConstant::String(string)
    }
}

impl From<CompiledFunction> for CompiledConstant {
    fn from(func: CompiledFunction) -> CompiledConstant {
        CompiledConstant::Function(Rc::new(func))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum NewValue {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
    HeapValue(HeapLoc),
}

#[derive(Debug, Clone, PartialEq)]
pub struct HeapLoc {
    index: usize,
}

#[derive(Debug, PartialEq)]
pub enum HeapValue {
    Class(Class),
    Instance(Instance),
    BoundMethod(BoundMethod),
    Closure(Closure),
    NativeFunction(NativeFunction),
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
    //Function(Rc<Function>),       // compile time representation of a function or method
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
                //Self::Function(value) => value.name.clone(),
                Self::Closure(value) => value.function.name.clone(),
                Self::NativeFunction(value) => value.name.clone(),
            }
        )
    }
}

impl From<&CompiledConstant> for Value {
    fn from(constant: &CompiledConstant) -> Value {
        match constant {
            CompiledConstant::Bool(value) => Value::Bool(*value),
            CompiledConstant::Nil => Value::Nil,
            CompiledConstant::Number(value) => Value::Number(*value),
            CompiledConstant::String(value) => Value::String(value.clone()),
            CompiledConstant::Function(_) => panic!("Compiled function must go through CLOSURE instruction to create runtime value."),
        }
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

// impl From<Function> for Value {
//     fn from(func: Function) -> Value {
//         Value::Function(Rc::new(func))
//     }
// }

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
pub struct Closure {
    pub function: Rc<CompiledFunction>,
    pub upvalues: Vec<Rc<Upvalue>>,
}

#[derive(Debug, PartialEq)]
pub struct Upvalue {
    pub stack_index: usize, // the location in the value stack if this upvalue has not yet been closed
    pub closed: RefCell<Option<Value>>, // the closed value once it has been moved to the heap
}

pub struct NativeFunction {
    pub arity: u8,
    pub func: fn(arguments: &[Value]) -> BasicResult<Value>,
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
