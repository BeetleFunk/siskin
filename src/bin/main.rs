use siskin::error;
use siskin::Environment;

use std::env;
use std::fs;
use std::io;

fn main() -> siskin::ExecutionResult {
    let mut args: Vec<String> = env::args().collect();
    println!("{:?}", args);

    // hack for running with cargo (or debugger) default args
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

fn run_file(path: &str) -> siskin::ExecutionResult {
    println!("Running file: {path}");
    let contents = fs::read_to_string(path)?;
    siskin::execute(&contents, &mut Environment::new(&mut io::stdout().lock()))
}

fn run_prompt() -> siskin::ExecutionResult {
    println!("Welcome to interactive prompt.");

    let mut buffer = String::new();
    let stdin = io::stdin();

    let mut output_writer = io::stdout().lock();
    let mut env = Environment::new(&mut output_writer);

    loop {
        stdin.read_line(&mut buffer)?;
        let result = siskin::execute(&buffer, &mut env);
        if let Err(error) = result {
            println!("*** Encountered an error during execution ***");
            println!("{error}");
            println!("");
        }
        buffer.clear();
    }
}
