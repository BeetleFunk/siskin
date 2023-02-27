mod scanner;

use crate::scanner::scan_tokens;

use std::env;
use std::fs;
use std::io;

type InterpreterResult = io::Result<()>;

fn main() -> InterpreterResult {
    let mut args: Vec<String> = env::args().collect();
    println!("{:?}", args);

    // hack for running with cargo default args
    if args.len() >= 1 && args[0] == "target/debug/siskin" {
        args.remove(0);
    }

    if args.len() > 1 {
        println!("Usage: siskin [script]");
        Err(io::Error::new(
            io::ErrorKind::Other,
            "Too many input arguments",
        ))
    } else if args.len() == 1 {
        run_file(&args[0])
    } else {
        run_prompt()
    }
}

fn run_file(path: &str) -> InterpreterResult {
    println!("Running file: {path}");
    let contents = fs::read_to_string(path)?;
    run(&contents)?;
    Ok(())
}

fn run_prompt() -> InterpreterResult {
    println!("Welcome to interactive prompt.");

    let mut buffer = String::new();
    let stdin = io::stdin();

    loop {
        stdin.read_line(&mut buffer)?;
        run(&buffer)?;
        buffer.clear();
    }
}

fn run(code: &str) -> InterpreterResult {
    println!("running code: {code}");

    let tokens = scan_tokens(code);

    for token in tokens {
        println!("{0}, {1}", token.lexeme, token.line);
    }

    Ok(())
}
