use crate::error::BasicError;
use crate::error::GenericResult;
use crate::expr::LiteralValue;
use crate::scanner::Token;
use crate::stmt::Stmt;

use std::collections::HashMap;
use std::fmt;
use std::io::Write;
use std::rc::Rc;

static DIAGNOSTICS: bool = false;

#[derive(Clone)]
pub struct NativeFunc {
    function: Rc<dyn Fn() -> SiskinValue>,
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
    id: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum SiskinValue {
    Function(SiskinFunction),
    FunctionHandle(Reference),
    Literal(LiteralValue),
}

#[derive(Debug, Clone, PartialEq)]
pub struct SiskinFunction {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Stmt,
    pub captured_vars: HashMap<String, Reference>,
}

// #[derive(Debug, Clone, PartialEq)]
enum HeapValue {
    Function(SiskinFunction),
    NativeFunction(NativeFunc),
    Literal(LiteralValue),
}

struct HeapObject {
    value: HeapValue,
    ref_count: usize,
}

impl fmt::Display for SiskinValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Function { .. } => "Function".to_string(),
                Self::FunctionHandle { .. } => "FunctionHandle".to_string(),
                Self::Literal(value) => value.to_string(),
            }
        )
    }
}

impl From<LiteralValue> for SiskinValue {
    fn from(value: LiteralValue) -> SiskinValue {
        SiskinValue::Literal(value)
    }
}

pub struct Environment<'a> {
    current_refid: usize,
    heap: HashMap<Reference, HeapObject>,
    stack: Vec<HashMap<String, Reference>>,
    pub output_writer: &'a mut dyn Write,
}

impl<'a> Environment<'a> {
    pub fn new(output_writer: &'a mut dyn Write) -> Environment<'a> {
        // initialize the global environment map as the first entry on the stack
        Environment {
            current_refid: 0,
            heap: HashMap::new(),
            stack: vec![HashMap::new()],
            output_writer,
        }
    }

    fn decrement_refcount(&mut self, reference: &Reference) {
        let object = self
            .heap
            .get_mut(reference)
            .expect("Stack frame was holding a reference not found on the heap.");
        // if count would go to zero, remove the entry entirely
        if object.ref_count == 1 {
            if DIAGNOSTICS { writeln!(self.output_writer, "Removing object from heap: {}", reference.id).expect("Writing to program output should always succeed."); }

            // remove the object first to take ownership and release the mutable borrow
            let object = self.heap.remove(reference).expect("Object should be guaranteed to exist in release phase.");
            if let HeapValue::Function(func) = &object.value {
                for capture in func.captured_vars.values() {
                    self.decrement_refcount(capture)
                }
            }
        } else {
            if DIAGNOSTICS { writeln!(self.output_writer, "Decrementing reference count (new refcount = {}) on object: {}", object.ref_count - 1, reference.id).expect("Writing to program output should always succeed."); }
            object.ref_count -= 1;
        }
    }

    fn add_or_create_reference(&mut self, value: SiskinValue) -> Reference {
        match value {
            SiskinValue::FunctionHandle(reference) => {
                let object = self
                    .heap
                    .get_mut(&reference)
                    .expect("Function handle was holding a reference not found on the heap.");
                object.ref_count = object.ref_count + 1;
                reference
            }
            SiskinValue::Literal(value) => {
                let reference = Reference {
                    id: self.current_refid,
                };
                self.current_refid += 1;
                self.heap.insert(
                    reference.clone(),
                    HeapObject {
                        value: HeapValue::Literal(value),
                        ref_count: 1,
                    },
                );
                reference
            }
            SiskinValue::Function(func) => {
                // increment ref count for all captured vars
                for captured in func.captured_vars.values() {
                    let object = self
                        .heap
                        .get_mut(&captured)
                        .expect("Captured variables included a reference that was not on the heap.");
                    object.ref_count += 1;
                }
                // create the new heap object for the function
                let reference = Reference {
                    id: self.current_refid,
                };
                self.current_refid += 1;
                self.heap.insert(
                    reference.clone(),
                    HeapObject {
                        value: HeapValue::Function(func),
                        ref_count: 1,
                    },
                );
                reference
            }
        }
    }

    pub fn push(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        let frame = self
            .stack
            .pop()
            .expect("Attempted to pop empty Environment stack."); // panic here because this would indicate a bug in the interpreter

        // update ref counts and remove unused vars
        for (var_name, reference) in frame {
            if DIAGNOSTICS { writeln!(self.output_writer, "Removing stack var: {}", var_name).expect("Writing to program output should always succeed."); }
            self.decrement_refcount(&reference);
        }
    }

    pub fn define(&mut self, name: String, value: SiskinValue) {
        let reference = self.add_or_create_reference(value);
        let frame = self
            .stack
            .last_mut()
            .expect("Missing global scope frame from Environment stack."); // panic here because this would indicate a bug in the interpreter
        let previous_entry = frame.insert(name, reference);
        if let Some(replaced_ref) = previous_entry {
            self.decrement_refcount(&replaced_ref);
        }
    }

    pub fn assign(&mut self, name: String, value: SiskinValue) -> GenericResult<()> {
        let reference = self.add_or_create_reference(value);
        if let Some(frame) = self.stack.iter_mut().rev().find(|x| x.contains_key(&name)) {
            let replaced_ref = frame
                .insert(name, reference)
                .expect("Check should have prevented assigning to uninitialized variable.");
            self.decrement_refcount(&replaced_ref);
            Ok(())
        } else {
            self.decrement_refcount(&reference);
            Err(Box::new(BasicError::new(&format!(
                "Undefined variable ({name})"
            ))))
        }
    }

    pub fn get(&self, name: &str) -> Option<SiskinValue> {
        for frame in self.stack.iter().rev() {
            if let Some(reference) = frame.get(name) {
                let object = self
                    .heap
                    .get(reference)
                    .expect("Stack frame was holding a reference not found on the heap.");
                let value = match &object.value {
                    HeapValue::Literal(literal) => SiskinValue::Literal(literal.clone()),
                    HeapValue::Function(_) | HeapValue::NativeFunction(_) => {
                        SiskinValue::FunctionHandle(reference.clone())
                    }
                };
                return Some(value);
            }
        }
        None
    }

    // TODO: can this special case be avoided with a standard interface for interpreter to read reference versus value?
    pub fn get_function(&self, handle: &SiskinValue) -> SiskinFunction {
        if let SiskinValue::FunctionHandle(reference) = handle {
            let object = self
                .heap
                .get(reference)
                .expect("Function handle was holding a reference not found on the heap.");
            match &object.value {
                HeapValue::Function(func) => func.clone(),
                _ => panic!("Not implemented function type for heap value."),
            }
        } else {
            panic!("Attempt to use invalid function handle.");
        }
    }
}
