use std::collections::HashMap;
use std::fmt;
use std::io::Write;

use super::expr::LiteralValue;
use super::stmt::Stmt;
use crate::error::{BasicError, GenericResult};
use crate::scanner::Token;

static DIAGNOSTICS: bool = false;

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

enum HeapValue {
    Function(SiskinFunction),
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
            if DIAGNOSTICS {
                writeln!(self.output_writer, "Removing object from heap: {}", reference.id)
                    .expect("Couldn't write to program output");
            }

            // remove the object first to take ownership and release the mutable borrow
            let object = self
                .heap
                .remove(reference)
                .expect("Object should be guaranteed to exist in release phase.");
            if let HeapValue::Function(func) = &object.value {
                for capture in func.captured_vars.values() {
                    self.decrement_refcount(capture)
                }
            }
        } else {
            if DIAGNOSTICS {
                writeln!(
                    self.output_writer,
                    "Decrementing reference count (new refcount = {}) on object: {}",
                    object.ref_count - 1,
                    reference.id
                )
                .expect("Couldn't write to program output");
            }
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
                object.ref_count += 1;
                reference
            }
            SiskinValue::Literal(value) => {
                let reference = Reference { id: self.current_refid };
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
                        .get_mut(captured)
                        .expect("Captured variables included a reference that was not on the heap.");
                    object.ref_count += 1;
                }
                // create the new heap object for the function
                let reference = Reference { id: self.current_refid };
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
        let frame = self.stack.pop().expect("Attempted to pop empty Environment stack."); // panic here because this would indicate a bug in the interpreter

        // update ref counts and remove unused vars
        for (var_name, reference) in frame {
            if DIAGNOSTICS {
                writeln!(self.output_writer, "Removing stack var: {}", var_name)
                    .expect("Couldn't write to program output");
            }
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

    // TODO: big hacks in here to make function variable capture work correctly, plenty of potential for improvement
    pub fn assign(&mut self, name: String, value: SiskinValue) -> GenericResult<()> {
        if let Some(reference) = self.get_reference(&name) {
            let new_value = match value {
                SiskinValue::Literal(literal) => HeapValue::Literal(literal),
                SiskinValue::Function(function) => HeapValue::Function(function),
                SiskinValue::FunctionHandle(handle) => {
                    let object = self
                        .heap
                        .get(&handle)
                        .expect("Function handle was holding a reference not found on the heap.");
                    match &object.value {
                        HeapValue::Function(func) => {
                            let func_clone = func.clone();
                            // increment ref count for all captured vars
                            for captured in func_clone.captured_vars.values() {
                                let object = self
                                    .heap
                                    .get_mut(captured)
                                    .expect("Captured variables included a reference that was not on the heap.");
                                object.ref_count += 1;
                            }
                            HeapValue::Function(func_clone)
                        }
                        _ => panic!("Not implemented function type for heap value."),
                    }
                }
            };

            let entry = self.heap.get_mut(&reference).expect("Mapped reference should exist.");

            let cleanup_refs: Option<Vec<Reference>> = if let HeapValue::Function(func) = &entry.value {
                let mut captured = Vec::new();
                for capture in func.captured_vars.values() {
                    captured.push(capture.clone());
                }
                Option::Some(captured)
            } else {
                Option::None
            };

            entry.value = new_value;

            if let Some(cleanup_refs) = cleanup_refs {
                cleanup_refs
                    .iter()
                    .for_each(|reference| self.decrement_refcount(reference));
            }

            Ok(())
        } else {
            Err(Box::new(BasicError::new(&format!("Undefined variable ({name})"))))
        }
    }

    pub fn get(&self, name: &str) -> Option<SiskinValue> {
        if let Some(reference) = self.get_reference(name) {
            let object = self
                .heap
                .get(&reference)
                .expect("Stack frame was holding a reference not found on the heap.");
            let value = match &object.value {
                HeapValue::Literal(literal) => SiskinValue::Literal(literal.clone()),
                HeapValue::Function(_) => SiskinValue::FunctionHandle(reference.clone()),
            };
            Some(value)
        } else {
            None
        }
    }

    pub fn get_reference(&self, name: &str) -> Option<Reference> {
        for frame in self.stack.iter().rev() {
            if let Some(reference) = frame.get(name) {
                return Some(reference.clone());
            }
        }
        None
    }

    pub fn capture_reference(&mut self, name: String, reference: Reference) {
        let object = self
            .heap
            .get_mut(&reference)
            .expect("Captured reference must exist on the heap.");

        let frame = self
            .stack
            .last_mut()
            .expect("There should always be at least one scope frame on Environment stack."); // panic here because this would indicate a bug in the interpreter
        let previous_entry = frame.insert(name, reference);
        if previous_entry.is_some() {
            panic!("Illegal capture of a reference under a symbol name that already exists.");
        }

        // increment the count after placing the new entry on the stack
        object.ref_count += 1;
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
