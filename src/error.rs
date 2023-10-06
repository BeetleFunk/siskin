use std::error::Error;
use std::fmt;
use std::result;

pub type GenericResult<T> = result::Result<T, Box<dyn Error>>;
pub type BasicResult<T> = result::Result<T, BasicError>;

#[derive(Debug)]
pub struct BasicError {
    pub description: String,
}

impl BasicError {
    pub fn new(description: &str) -> BasicError {
        BasicError {
            description: description.to_string(),
        }
    }
}

impl fmt::Display for BasicError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.description)
    }
}

impl Error for BasicError {}
