mod code;
mod compiler;
mod gc;
mod value;
mod vm;

use std::io::Write;

use crate::error;

pub fn execute(code: &str, output: &mut dyn Write) -> error::BasicResult<()> {
    let compiled = compiler::compile(code)?;
    vm::interpret(compiled, output)?;
    Ok(())
}
