use std::env;
use std::process::ExitCode;

fn main() -> ExitCode {
    let mut args: Vec<String> = env::args().collect();
    println!("{:?}", args);

    // hack for running with cargo defaults
    if args.len() >= 1 && args[0] == "target/debug/siskin" {
        args.remove(0);
    }

    if args.len() > 1 {
        println!("Usage: siskin [script]");
        return ExitCode::from(64);
    } else if args.len() == 1 {
        run_file(&args[0]);
    } else {
        run_prompt();
    }

    return ExitCode::SUCCESS;
}

fn run_file(path: &str) {
    println!("Running file: {path}");
}

fn run_prompt() {
    println!("Welcome to interactive prompt.");
}
