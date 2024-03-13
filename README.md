# Siskin - A Lox Interpreter Implemented in Rust
Inspired by Robert Nystrom's book, [_Crafting Interpreters_](https://craftinginterpreters.com/).

This project began as a way to learn Rust and explore the programming language topics covered in _Crafting Interpreters_. The book walks the reader through the creation of both a tree-walk interpreter implemented in Java (jlox) and a bytecode virtual machine implemented in C (clox). Lox is the name given to the scripting language that runs on these interpreters.

My implementation here diverges from the exact design in the book because I wanted to better understand Rust's strengths and the tradeoffs compared to other languages. The jlox interpreter makes use of Java's garbage collector and object oriented programming techniques like inheritance, both of which are unavailable in Rust. The clox interpreter relies on sharing raw pointers and mutating memory in a way that is not compatible with the ownership and borrowing rules of safe Rust. Solving these design challenges was part of the learning experience.

**NOTE**
This codebase is a learning project and a work in progress. The bytecode virtual machine is more polished and supports the full feature set covered in the book and a bit more. The tree-walk interpreter came earlier in the learning process and also doesn't have support for classes or inheritance.

## Building
Use `cargo build` to build the project package which contains a single library crate and two binary crates.

The library crate defined in `lib.rs` contains the implementation for both types of interpreters. The `bytecode` module provides an API for compiling and running scripts on the bytecode virtual machine, and the `treewalk` module provides similar functionality for the tree-walk interpreter.

The `bytecode` and `treewalk` binary crates run the corresponding interpreter variant.

## Running
`cargo run` will default to running the bytecode virtual machine.

The `bytecode` and `treewalk` binary crates will run the corresponding interpreter in interactive (REPL) mode by default. Passing a single file path argument when running the binary will execute the given script file and then exit.
```
cargo run --bin bytecode (SCRIPT_FILE)

cargo run --bin treewalk (SCRIPT_FILE)
```

## Testing
Automated testing is accomplished with a suite of integration tests that execute scripts on the interpreter and confirm that program output matches expected patterns.

There are separate tests for each interpreter variant as well as tests for the garbage collection subsystem of the bytecode virtual machine. All integration tests are located in the `tests` directory. The test suite covers basic language features, runtime error reporting, and more complex scenarios such as deeply nested function variable captures (upvalues).

Use `cargo test` to run all of the tests.

## Code Formatting
Code formatting uses Rustfmt and is configured in `rustfmt.toml`.

Rustfmt's import grouping features are not yet supported in the stable toolchain, so it is required that you install a nightly toolchain before running the formatter. To setup the toolchain run:
```
rustup toolchain install nightly
```

Once you have the nightly toolchain installed, install Rustfmt with:
```
rustup component add rustfmt --toolchain nightly
```

You should now be able to run the formatter with the command:
```
cargo +nightly fmt
```
