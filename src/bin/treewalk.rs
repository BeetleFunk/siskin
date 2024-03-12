use std::borrow::Borrow;
use std::{env, fs, io};

use siskin::treewalk::environment::Environment;
use siskin::treewalk::ExecutionResult;
use siskin::{error, treewalk};

fn main() -> error::GenericResult<()> {
    let mut args = env::args();
    args.next()
        .expect("Program arguments should have at least one entry (typically the path of executable by default)");

    if args.len() > 1 {
        println!("Usage: treewalk [SCRIPT_PATH]");
        Err(Box::new(error::BasicError::new("Too many input arguments")))
    } else if let Some(path) = args.next() {
        run_file(&path)
    } else {
        run_prompt()
    }
}

fn run_file(path: &str) -> ExecutionResult {
    println!("Running script from file at '{path}'");
    let contents = fs::read_to_string(path)?;
    let result = treewalk::execute(&contents, &mut Environment::new(&mut io::stdout().lock()));
    if let Err(error) = &result {
        let error: &dyn std::error::Error = error.borrow();
        display_error(error);
    }
    result
}

fn run_prompt() -> ExecutionResult {
    println!("Welcome to the interactive prompt for the Siskin interpreter (treewalk variant).\n");

    let stdin = io::stdin();
    let mut output_writer = io::stdout().lock();
    let mut env = Environment::new(&mut output_writer);
    let mut buffer = String::new();
    loop {
        stdin.read_line(&mut buffer)?;
        let result = treewalk::execute(&buffer, &mut env);
        if let Err(error) = &result {
            let error: &dyn std::error::Error = error.borrow();
            display_error(error);
        }
        buffer.clear();
    }
}

fn display_error(error: impl std::error::Error) {
    println!(" *** An error occurred while running the script ***");
    println!("   {error}\n");
}
