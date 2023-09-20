mod code;
mod compiler;
mod vm;

pub fn execute() {
    let mut chunk = code::Chunk::new();
    
    chunk.write_op(code::OpCode::Return, 132);

    let index = chunk.add_constant(code::Value::Number(2.5));
    chunk.write_op(code::OpCode::Constant, 132);
    chunk.write(index, 132);
    
    code::disassemble_chunk(&chunk, "test chunk");
}
