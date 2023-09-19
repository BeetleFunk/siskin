mod expr;
mod interpreter;
mod parser;
mod resolver;
mod stmt;

pub mod environment;

use crate::error;
use crate::scanner;

pub type ExecutionResult = error::GenericResult<()>;

pub fn execute(code: &str, env: &mut environment::Environment) -> ExecutionResult {
    let tokens = scanner::scan_tokens(code)?;
    let statements = parser::parse(&tokens)?;
    interpreter::execute(&statements, env)?;
    Ok(())
}
