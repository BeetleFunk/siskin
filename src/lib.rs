mod expr;
mod interpreter;
mod parser;
mod scanner;
mod stmt;

pub mod error;

pub use interpreter::Environment;

pub type ExecutionResult = error::GenericResult<()>;

pub fn execute(code: &str, env: &mut interpreter::Environment) -> ExecutionResult {
    let tokens = scanner::scan_tokens(code)?;
    let statements = parser::parse(&tokens)?;
    interpreter::execute(&statements, env)
}
