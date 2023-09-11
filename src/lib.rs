mod environment;
mod expr;
mod interpreter;
mod parser;
mod resolver;
mod scanner;
mod stmt;

pub mod error;

pub use environment::Environment;

pub type ExecutionResult = error::GenericResult<()>;

pub fn execute(code: &str, env: &mut Environment) -> ExecutionResult {
    let tokens = scanner::scan_tokens(code)?;
    let statements = parser::parse(&tokens)?;
    interpreter::execute(&statements, env)?;
    Ok(())
}
