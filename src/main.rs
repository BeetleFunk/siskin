mod error;
mod expr;
mod interpreter;
mod parser;
mod scanner;
mod stmt;

use crate::interpreter::Environment;
use crate::parser::parse;
use crate::scanner::scan_tokens;

use std::env;
use std::fs;
use std::io;

type ExecutionResult = error::GenericResult<()>;

fn main() -> ExecutionResult {
    let mut args: Vec<String> = env::args().collect();
    println!("{:?}", args);

    // hack for running with cargo default args
    args.remove(0);
    // if args.len() >= 1 && args[0] == "target/debug/siskin" {
    //     args.remove(0);
    // }

    if args.len() > 1 {
        println!("Usage: siskin [script]");
        Err(Box::new(error::BasicError::new("Too many input arguments")))
    } else if args.len() == 1 {
        run_file(&args[0])
    } else {
        run_prompt()
    }
}

fn run_file(path: &str) -> ExecutionResult {
    println!("Running file: {path}");
    let contents = fs::read_to_string(path)?;
    let mut env = Environment::new();
    run(&contents, &mut env)
}

fn run_prompt() -> ExecutionResult {
    println!("Welcome to interactive prompt.");

    let mut buffer = String::new();
    let stdin = io::stdin();
    let mut env = Environment::new();

    loop {
        stdin.read_line(&mut buffer)?;
        let result = run(&buffer, &mut env);
        if let Err(error) = result {
            println!("*** Encountered an error during execution ***");
            println!("{error}");
            println!("");
        }
        buffer.clear();
    }
}

fn run(code: &str, env: &mut Environment) -> ExecutionResult {
    let tokens = scan_tokens(code)?;
    let statements = parse(&tokens)?;
    interpreter::execute(&statements, env)
}
