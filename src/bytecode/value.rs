use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use enum_as_inner::EnumAsInner;

use crate::error::BasicResult;

use super::code::{CompiledConstant, CompiledFunction};

const TRACE_VALUE_DROP: bool = false;

#[derive(Debug, Clone, EnumAsInner, PartialEq)]
pub enum Value {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
    HeapRef(HeapRef),
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

impl From<HeapRef> for Value {
    fn from(location: HeapRef) -> Value {
        Value::HeapRef(location)
    }
}

impl From<&CompiledConstant> for Value {
    fn from(constant: &CompiledConstant) -> Value {
        match constant {
            CompiledConstant::Bool(value) => Value::Bool(*value),
            CompiledConstant::Number(value) => Value::Number(*value),
            CompiledConstant::String(value) => Value::String(value.clone()),
            CompiledConstant::Function(_) => panic!(
                "Compiled function must go through CLOSURE instruction to create runtime value."
            ),
        }
    }
}

impl Value {
    pub fn to_string(&self, heap: &[HeapEntry]) -> String {
        match self {
            Value::Bool(value) => value.to_string(),
            Value::Nil => "Nil".to_string(),
            Value::Number(value) => value.to_string(),
            Value::String(value) => value.clone(),
            Value::HeapRef(location) => {
                let heap_entry = &heap[location.0];
                heap_entry.value.to_string()
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HeapRef(pub usize);

impl fmt::Display for HeapRef {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

pub struct HeapEntry {
    pub marked: Cell<bool>,
    pub value: HeapValue,
}

#[derive(Debug, EnumAsInner, PartialEq)]
pub enum HeapValue {
    Class(Class),
    Instance(Instance),
    BoundMethod(BoundMethod),
    Closure(Rc<Closure>), // Rc to share ownership with call stack
    NativeFunction(NativeFunction),
}

impl fmt::Display for HeapValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Class(v) => write!(f, "{}", &v.name),
            Self::Instance(v) => write!(f, "{} instance", &v.debug_class_name),
            Self::BoundMethod(v) => write!(
                f,
                "{} closure bound to instance at {}",
                v.closure, v.instance
            ),
            Self::Closure(v) => write!(f, "{}", &v.function.name),
            Self::NativeFunction(v) => write!(f, "{}", &v.name),
        }
    }
}

impl From<Class> for HeapValue {
    fn from(class: Class) -> HeapValue {
        HeapValue::Class(class)
    }
}

impl From<Instance> for HeapValue {
    fn from(instance: Instance) -> HeapValue {
        HeapValue::Instance(instance)
    }
}

impl From<BoundMethod> for HeapValue {
    fn from(method: BoundMethod) -> HeapValue {
        HeapValue::BoundMethod(method)
    }
}

impl From<Closure> for HeapValue {
    fn from(closure: Closure) -> HeapValue {
        HeapValue::Closure(Rc::new(closure))
    }
}

impl From<NativeFunction> for HeapValue {
    fn from(func: NativeFunction) -> HeapValue {
        HeapValue::NativeFunction(func)
    }
}

#[derive(Debug, PartialEq)]
pub struct Class {
    pub name: String,
    pub methods: HashMap<String, HeapRef>,
}

impl<'a> TryFrom<&'a HeapValue> for &'a Class {
    type Error = ();
    fn try_from(value: &HeapValue) -> Result<&Class, ()> {
        if let HeapValue::Class(class) = value {
            Ok(class)
        } else {
            Err(())
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Instance {
    pub debug_class_name: String, // temporary class name copy for easier debugging
    pub class: HeapRef,
    pub fields: HashMap<String, Value>,
}

impl<'a> TryFrom<&'a HeapValue> for &'a Instance {
    type Error = ();
    fn try_from(value: &HeapValue) -> Result<&Instance, ()> {
        if let HeapValue::Instance(instance) = value {
            Ok(instance)
        } else {
            Err(())
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct BoundMethod {
    pub instance: HeapRef,
    pub closure: HeapRef,
}

#[derive(Debug, PartialEq)]
pub struct Closure {
    pub function: Rc<CompiledFunction>,
    pub upvalues: Vec<Rc<Upvalue>>,
}

impl<'a> TryFrom<&'a HeapValue> for &'a Closure {
    type Error = ();
    fn try_from(value: &HeapValue) -> Result<&Closure, ()> {
        if let HeapValue::Closure(closure) = value {
            Ok(closure)
        } else {
            Err(())
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Upvalue {
    pub stack_index: usize, // the location in the value stack if this upvalue has not yet been closed
    pub closed: RefCell<Option<Value>>, // the closed value once it has been moved to the heap
}

pub struct NativeFunction {
    pub arity: u8,
    pub func: fn(heap: &[HeapEntry], arguments: &[Value]) -> BasicResult<Value>,
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
                self.methods.len()
            );
        }
    }
}

impl Drop for Instance {
    fn drop(&mut self) {
        if TRACE_VALUE_DROP {
            println!(
                "Dropping instance of {} with {} fields",
                self.debug_class_name,
                self.fields.len()
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

impl Drop for BoundMethod {
    fn drop(&mut self) {
        if TRACE_VALUE_DROP {
            println!(
                "Dropping bound method - instance at {}, closure at {}",
                self.instance, self.closure
            );
        }
    }
}
