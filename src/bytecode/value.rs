use std::cell::Cell;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;

use crate::error::BasicResult;

use super::code::{CompiledConstant, CompiledFunction};

const TRACE_VALUE_DROP: bool = false;

#[derive(Debug, Clone, PartialEq)]
pub enum NewValue {
    Bool(bool),
    Nil,
    Number(f64),
    String(String),
    HeapValue(HeapRef),
}

impl From<bool> for NewValue {
    fn from(boolean: bool) -> NewValue {
        NewValue::Bool(boolean)
    }
}

impl From<f64> for NewValue {
    fn from(number: f64) -> NewValue {
        NewValue::Number(number)
    }
}

impl From<String> for NewValue {
    fn from(string: String) -> NewValue {
        NewValue::String(string)
    }
}

impl From<HeapRef> for NewValue {
    fn from(location: HeapRef) -> NewValue {
        NewValue::HeapValue(location)
    }
}

impl From<&CompiledConstant> for NewValue {
    fn from(constant: &CompiledConstant) -> NewValue {
        match constant {
            CompiledConstant::Bool(value) => NewValue::Bool(*value),
            CompiledConstant::Number(value) => NewValue::Number(*value),
            CompiledConstant::String(value) => NewValue::String(value.clone()),
            CompiledConstant::Function(_) => panic!(
                "Compiled function must go through CLOSURE instruction to create runtime value."
            ),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct HeapRef {
    pub index: usize,
}

pub struct HeapEntry {
    pub marked: Cell<bool>,
    pub value: HeapValue
}

#[derive(Debug, PartialEq)]
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
                v.closure.index, v.instance.index
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

#[derive(Debug, PartialEq)]
pub struct Instance {
    pub debug_class_name: String, // temporary class name copy for easier debugging
    pub class: HeapRef,
    pub fields: HashMap<String, NewValue>,
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

#[derive(Debug, PartialEq)]
pub struct Upvalue {
    pub stack_index: usize, // the location in the value stack if this upvalue has not yet been closed
    pub closed: RefCell<Option<NewValue>>, // the closed value once it has been moved to the heap
}

pub struct NativeFunction {
    pub arity: u8,
    pub func: fn(heap: &[HeapEntry], arguments: &[NewValue]) -> BasicResult<NewValue>,
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
                self.instance.index, self.closure.index
            );
        }
    }
}
