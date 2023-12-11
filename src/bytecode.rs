mod code;
mod compiler;
mod value;
mod vm;

use std::io::Write;

use crate::error;

pub fn execute(code: &str, output: &mut dyn Write) -> error::BasicResult<()> {
    vm::interpret(code, output)?;
    Ok(())
}

// pub fn execute() {
//     let mut chunk = code::Chunk::new();
    
//     let index = chunk.add_constant(code::Value::Number(1.2));
//     chunk.write_op(code::OpCode::Constant, 132);
//     chunk.write(index, 132);

//     let index = chunk.add_constant(code::Value::Number(3.4));
//     chunk.write_op(code::OpCode::Constant, 132);
//     chunk.write(index, 132);

//     chunk.write_op(code::OpCode::Add, 132);

//     let index = chunk.add_constant(code::Value::Number(5.6));
//     chunk.write_op(code::OpCode::Constant, 132);
//     chunk.write(index, 132);

//     chunk.write_op(code::OpCode::Divide, 132);

//     chunk.write_op(code::OpCode::Negate, 132);

//     chunk.write_op(code::OpCode::Return, 132);
    
//     //code::disassemble_chunk(&chunk, "test chunk");

//     vm::interpret(&chunk).expect("no good");
// }
