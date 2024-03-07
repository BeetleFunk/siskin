mod code;
mod compiler;
mod gc;
mod stdlib;
mod value;
mod vm;

use std::io::Write;

use crate::error;

pub fn create_vm() -> vm::State {
    vm::State::new()
}

// compiles and then runs code on the provided bytecode virtual machine
pub fn interpret(vm_state: &mut vm::State, code: &str, output: &mut dyn Write) -> error::BasicResult<()> {
    let compiled = compiler::compile(code)?;
    vm::interpret(vm_state, compiled, output)?;
    Ok(())
}

// compiles and then runs code on a fresh bytecode virtual machine that is dropped at the end of the function
pub fn interpret_ephemeral(code: &str, output: &mut dyn Write) -> error::BasicResult<()> {
    let mut vm_state = vm::State::new();
    interpret(&mut vm_state, code, output)
}
