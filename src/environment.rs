use crate::error::BasicError;
use crate::error::GenericResult;
use crate::expr::LiteralValue;
use crate::scanner::Token;
use crate::stmt::Stmt;

use std::collections::HashMap;
use std::fmt;
use std::io::Write;
use std::rc::Rc;

#[derive(Clone)]
pub struct NativeFunc {
    function: Rc<dyn Fn() -> SiskinValue>
}

impl fmt::Debug for NativeFunc {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // TODO: implement this for real
        write!(f, "Native function debug view")
    }
}

impl PartialEq for NativeFunc {
    fn eq(&self, other: &Self) -> bool {
        // TODO: implement this for real
        false
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Reference {
    key: usize
}

#[derive(Debug, Clone, PartialEq)]
pub enum SiskinValue {
    FunctionHandle(Reference),
    Literal(LiteralValue),
}

#[derive(Debug)]
pub struct SiskinFunction {
    name: Token,
    params: Vec<Token>,
    body: Stmt,
    captured_vars: HashMap<String, Reference>
}

// #[derive(Debug, Clone, PartialEq)]
enum HeapValue {
    Function(SiskinFunction),
    NativeFunction(NativeFunc),
    Literal(LiteralValue),
}

pub struct HeapObject {
    value: HeapValue,
    ref_count: usize
}

impl fmt::Display for SiskinValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Self::FunctionHandle{..} => "FunctionHandle".to_string(),
            Self::Literal(value) => value.to_string(),
        })
    }
}

impl From<LiteralValue> for SiskinValue {
    fn from(value: LiteralValue) -> SiskinValue {
        SiskinValue::Literal(value)
    }
}

pub struct Environment<'a> {
    heap: HashMap<Reference, HeapObject>,
    stack: Vec<HashMap<String, Reference>>,
    pub output_writer: &'a mut dyn Write,
}

impl<'a> Environment<'a> {
    pub fn new(output_writer: &'a mut dyn Write) -> Environment<'a> {
        // initialize the global environment map as the first entry on the stack
        Environment {
            heap: HashMap::new(),
            stack: vec![HashMap::new()],
            output_writer,
        }
    }

    pub fn push(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        let frame = self.stack.pop().expect("Attempted to pop empty Environment stack."); // panic here because this would indicate a bug in the interpreter
        // update ref counts and remove unused vars
        for reference in frame.values() {
            let object = self.heap.get_mut(reference).expect("Stack frame was holding a reference not found on the heap.");
            if object.ref_count == 1 {
                if let HeapValue::Function(func) = object.value {
                    // TODO: recursive ref count cleanup for captured vars
                }
                self.heap.remove(reference);
            } else {
                object.ref_count = object.ref_count - 1;
            }
        }
    }

    pub fn define(&mut self, name: String, value: SiskinValue) {
        let frame = self
            .stack
            .last_mut()
            .expect("Missing global scope frame from Environment stack."); // panic here because this would indicate a bug in the interpreter
        frame.insert(name, value);
    }

    pub fn assign(&mut self, name: String, value: SiskinValue) -> GenericResult<()> {
        if let Some(frame) = self.stack.iter_mut().rev().find(|x| x.contains_key(&name)) {
            frame.insert(name, value);
            Ok(())
        } else {
            Err(Box::new(BasicError::new(&format!(
                "Undefined variable ({name})"
            ))))
        }
    }

    pub fn get(&self, name: &str) -> Option<&SiskinValue> {
        for frame in self.stack.iter().rev() {
            if let Some(value) = frame.get(name) {
                return Some(value);
            }
        }
        None
    }
}
