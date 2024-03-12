use std::{env, fs, io};

use siskin::bytecode;
use siskin::error::{self, BasicError};

fn main() -> error::GenericResult<()> {
    let mut args = env::args();
    args.next()
        .expect("Program arguments should have at least one entry (typically the path of executable by default)");

    if args.len() > 1 {
        println!("Usage: bytecode [SCRIPT_PATH]");
        Err(Box::new(error::BasicError::new("Too many input arguments")))
    } else if let Some(path) = args.next() {
        run_file(&path)
    } else {
        run_prompt()
    }
}

fn run_file(path: &str) -> error::GenericResult<()> {
    println!("Running script from file at '{path}'");
    let contents = fs::read_to_string(path)?;
    let result = bytecode::interpret_ephemeral(&contents, &mut io::stdout().lock());
    if let Err(error) = &result {
        display_error(error);
    }
    Ok(result?)
}

fn run_prompt() -> error::GenericResult<()> {
    println!("Welcome to the interactive prompt for the Siskin interpreter (bytecode variant).\n");

    let stdin = io::stdin();
    let mut vm_state = bytecode::create_vm();
    let mut output = io::stdout().lock();
    let mut buffer = String::new();
    loop {
        stdin.read_line(&mut buffer)?;
        let result = bytecode::interpret(&mut vm_state, &buffer, &mut output);
        if let Err(error) = &result {
            display_error(error);
        }
        buffer.clear();
    }
}

fn display_error(error: &BasicError) {
    println!(" *** An error occurred while running the script ***");
    println!("   {error}\n");
}
