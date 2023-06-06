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

#[derive(Debug, Clone, PartialEq)]
pub enum SiskinValue {
    Function{ name: Token, params: Vec<Token>, body: Stmt },
    NativeFunction(NativeFunc),
    Literal(LiteralValue),
}

impl fmt::Display for SiskinValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Self::Function{..} => "Function".to_string(),
            Self::NativeFunction(_) => "NativeFunction".to_string(),
            Self::Literal(value) => value.to_string(),
        })
    }
}

impl From<LiteralValue> for SiskinValue {
    fn from(value: LiteralValue) -> SiskinValue {
        SiskinValue::Literal(value)
    }
}

// #[derive(Debug, Clone, PartialEq)]
// pub struct Reference {
//     key: usize
// }

pub struct Environment<'a> {
    //data: HashMap<Reference, SiskinValue>,
    stack: Vec<HashMap<String, SiskinValue>>,
    pub output_writer: &'a mut dyn Write,
}

impl<'a> Environment<'a> {
    pub fn new(output_writer: &'a mut dyn Write) -> Environment<'a> {
        // initialize the global environment map as the first entry on the stack
        Environment {
            stack: vec![HashMap::new()],
            output_writer,
        }
    }

    pub fn push(&mut self) {
        self.stack.push(HashMap::new());
    }

    pub fn pop(&mut self) {
        self.stack.pop();
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
