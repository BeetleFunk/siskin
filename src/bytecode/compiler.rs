use std::mem;
use std::result;

use crate::error::{BasicError, BasicResult};
use crate::scanner::{Scanner, Token, TokenType};

use super::code::{self, Chunk, Function, OpCode, Value};

type UnitResult = BasicResult<()>;

const DEBUG_DUMP_CHUNK: bool = false;

struct Parser {
    scanner: Scanner,
    current: Token,
    previous: Token,
}

impl Parser {
    // automatically prime current with the first token, hence the possible error
    fn new(mut scanner: Scanner) -> result::Result<Self, BasicError> {
        let current = scanner.next_token()?;
        Ok(Parser {
            scanner,
            current,
            previous: Token {
                // this initial dummy token should never actually be read with look-ahead parsing
                token_type: TokenType::Eof,
                lexeme: "".to_string(),
                line: 0,
                token_value: None,
            },
        })
    }

    fn advance(&mut self) -> result::Result<&Token, BasicError> {
        let replaced = mem::replace(&mut self.current, self.scanner.next_token()?);
        self.previous = replaced;
        Ok(&self.current)
    }
}

struct Compiler {
    parser: Parser,
    func_stack: Vec<CompilerFunction>,
    locals: Vec<Local>,
    scope_depth: u32,
}

struct CompilerFunction {
    func: Function,
    locals_offset: usize,
}

struct Local {
    name: Token,
    depth: u32,
}

impl Compiler {
    fn new(parser: Parser) -> Compiler {
        // toplevel bytecode for a script will end up in this root function
        let root_func = Function {
            arity: 0,
            chunk: Chunk::new(),
            name: "".to_string(),
        };
        Compiler {
            parser,
            func_stack: vec![CompilerFunction {
                func: root_func,
                locals_offset: 0,
            }],
            locals: Vec::new(),
            scope_depth: 0,
        }
    }

    fn chunk(&self) -> &Chunk {
        &self.func_stack.last().unwrap().func.chunk
    }

    fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.func_stack.last_mut().unwrap().func.chunk
    }

    fn emit_op(&mut self, byte: OpCode) {
        let line = self.parser.previous.line;
        self.chunk_mut().write_op(byte, line);
    }

    fn emit_ops(&mut self, byte1: OpCode, byte2: OpCode) {
        let line = self.parser.previous.line;
        self.chunk_mut().write_op(byte1, line);
        self.chunk_mut().write_op(byte2, line);
    }

    fn emit_data_op(&mut self, byte1: OpCode, byte2: u8) {
        let line = self.parser.previous.line;
        self.chunk_mut().write_op(byte1, line);
        self.chunk_mut().write(byte2, line);
    }

    fn emit_data(&mut self, byte1: u8, byte2: u8) {
        let line = self.parser.previous.line;
        self.chunk_mut().write(byte1, line);
        self.chunk_mut().write(byte2, line);
    }

    fn advance_if_match(&mut self, token_type: TokenType) -> Result<bool, BasicError> {
        if self.parser.current.token_type == token_type {
            self.parser.advance()?;
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn consume(&mut self, expected: TokenType, error_msg: &str) -> Result<&Token, BasicError> {
        if self.advance_if_match(expected)? {
            Ok(&self.parser.previous)
        } else {
            Err(build_error(error_msg, self.parser.current.line))
        }
    }

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        while let Some(last) = self.locals.last() {
            if last.depth > self.scope_depth {
                self.locals.pop();
                self.emit_op(OpCode::Pop);
            } else {
                break;
            }
        }
    }

    fn add_local(&mut self, name: Token) {
        if self.locals.len() >= u8::MAX.into() {
            panic!("Locals in scope limited to 256 entries at the moment.");
        }

        self.locals.push(Local {
            name,
            depth: self.scope_depth,
        })
    }

    fn compute_local_index(&self, name: &str) -> Option<u8> {
        let base_offset = self.func_stack.last().unwrap().locals_offset;
        let available = &self.locals[base_offset..self.locals.len()];
        if let Some((match_index, _)) = available
            .iter()
            .enumerate()
            .rev()
            .find(|entry| entry.1.name.lexeme == name)
        {
            Some(match_index as u8)
        } else {
            None
        }
    }

    fn identifier_in_current_scope(&self, name: &str) -> bool {
        for local in self.locals.iter().rev() {
            // locals are always added in order of scope depth, so we can bail as soon as we hit one at a lower depth
            if local.depth != self.scope_depth {
                return false;
            } else if local.name.lexeme == name {
                return true;
            }
        }
        false
    }
}

// parsing precedence values
const PREC_NONE: u32 = 0;
const PREC_ASSIGNMENT: u32 = 1; // =
const PREC_OR: u32 = 2; // or
const PREC_AND: u32 = 3; // and
const PREC_EQUALITY: u32 = 4; // == !=
const PREC_COMPARISON: u32 = 5; // < > <= >=
const PREC_TERM: u32 = 6; // + -
const PREC_FACTOR: u32 = 7; // * /
const PREC_UNARY: u32 = 8; // ! -
const PREC_CALL: u32 = 9; // . ()

struct ParseRule {
    prefix: Option<fn(&mut Compiler, bool) -> UnitResult>,
    infix: Option<fn(&mut Compiler, bool) -> UnitResult>,
    precedence: u32,
}

// Each entry includes corresponding TokenType either for index sorting or for sanity checking if table is used directly
#[rustfmt::skip]
const PARSE_TABLE: [(TokenType, ParseRule); 39] = [
    (TokenType::LeftParen,      ParseRule { prefix: Some(grouping), infix: Some(call),      precedence: PREC_CALL }),
    (TokenType::RightParen,     ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::LeftBrace,      ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }), 
    (TokenType::RightBrace,     ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Comma,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Dot,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Minus,          ParseRule { prefix: Some(unary),    infix: Some(binary),    precedence: PREC_TERM }),
    (TokenType::Plus,           ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_TERM }),
    (TokenType::Semicolon,      ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Slash,          ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_FACTOR }),
    (TokenType::Star,           ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_FACTOR }),
    (TokenType::Bang,           ParseRule { prefix: Some(unary),    infix: None,            precedence: PREC_NONE }),
    (TokenType::BangEqual,      ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_EQUALITY }),
    (TokenType::Equal,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::EqualEqual,     ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_EQUALITY }),
    (TokenType::Greater,        ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_COMPARISON }),
    (TokenType::GreaterEqual,   ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_COMPARISON }),
    (TokenType::Less,           ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_COMPARISON }),
    (TokenType::LessEqual,      ParseRule { prefix: None,           infix: Some(binary),    precedence: PREC_COMPARISON }),
    (TokenType::Identifier,     ParseRule { prefix: Some(variable), infix: None,            precedence: PREC_NONE }),
    (TokenType::String,         ParseRule { prefix: Some(string),   infix: None,            precedence: PREC_NONE }),
    (TokenType::Number,         ParseRule { prefix: Some(number),   infix: None,            precedence: PREC_NONE }),
    (TokenType::And,            ParseRule { prefix: None,           infix: Some(logic_and), precedence: PREC_AND }),
    (TokenType::Class,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Else,           ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::False,          ParseRule { prefix: Some(literal),  infix: None,            precedence: PREC_NONE }),
    (TokenType::For,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Fun,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::If,             ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Nil,            ParseRule { prefix: Some(literal),  infix: None,            precedence: PREC_NONE }),
    (TokenType::Or,             ParseRule { prefix: None,           infix: Some(logic_or),  precedence: PREC_OR }),
    (TokenType::Print,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Return,         ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Super,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::This,           ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::True,           ParseRule { prefix: Some(literal),  infix: None,            precedence: PREC_NONE }),
    (TokenType::Var,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::While,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Eof,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE })];

fn get_rule(token_type: TokenType) -> &'static ParseRule {
    &PARSE_TABLE[token_type as usize].1
}

pub fn compile(source: &str) -> result::Result<Function, BasicError> {
    let parser = Parser::new(Scanner::new(source))?;
    let mut compiler = Compiler::new(parser);

    while compiler.parser.current.token_type != TokenType::Eof {
        declaration(&mut compiler)?;
    }

    emit_empty_return(&mut compiler);

    if DEBUG_DUMP_CHUNK {
        code::disassemble_chunk(&compiler.func_stack[0].func.chunk, "name");
    }

    Ok(compiler.func_stack.remove(0).func)
}

fn declaration(compiler: &mut Compiler) -> UnitResult {
    if compiler.advance_if_match(TokenType::Var)? {
        var_declaration(compiler)
    } else if compiler.advance_if_match(TokenType::Fun)? {
        fun_declaration(compiler)
    } else {
        statement(compiler)
    }
}

fn var_declaration(compiler: &mut Compiler) -> UnitResult {
    let identifier = compiler
        .consume(TokenType::Identifier, "Expect variable name.")?
        .clone();

    // compile the expression before adding the local variable entry, this makes variable shadowing work properly
    if compiler.advance_if_match(TokenType::Equal)? {
        expression(compiler)?;
    } else {
        compiler.emit_op(OpCode::Nil);
    }

    compiler.consume(
        TokenType::Semicolon,
        "Expect ';' after variable declaration.",
    )?;

    if compiler.scope_depth == 0 {
        // for global vars, save the variable name as a string in the constant table
        let index = compiler
            .chunk_mut()
            .add_constant(Value::from(identifier.extract_name().clone())); // TODO: how to avoid the clone here?
        define_global(compiler, index);
    } else {
        // for local vars, add the identifier name to the list of locals
        compiler.add_local(identifier);
    }

    Ok(())
}

fn fun_declaration(compiler: &mut Compiler) -> UnitResult {
    let identifier = compiler
        .consume(TokenType::Identifier, "Expect function name.")?
        .clone();
    let name = identifier.extract_name();
    let locals_offset = compiler.locals.len();

    if compiler.scope_depth == 0 {
        // for global scope, add a temporary empty entry to compiler locals to get the proper offset into the value stack
        // at runtime the function value being called will always be in stack slot zero (relative to the function call frame)
        let mut dummy_entry = identifier.clone();
        dummy_entry.lexeme = "".to_string();
        compiler.add_local(dummy_entry);
    } else {
        // for non-global scope, add the function to locals before proceeding to facilitate recursion
        // unlike variables, functions are not allowed to shadow existing names
        if compiler.identifier_in_current_scope(name) {
            build_error(
                &format!(
                    "Already a variable in scope matching the function name ({}).",
                    name
                ),
                identifier.line,
            );
        }
        compiler.add_local(identifier.clone());
    }

    function(compiler, name.clone(), locals_offset)?;

    if compiler.scope_depth == 0 {
        // if in global scope, the temporary entry in locals must be popped off after compiling the function (see above)
        let dummy_entry = compiler.locals.pop().unwrap();
        if !dummy_entry.name.lexeme.is_empty() {
            // if we are popping a non-temporary local then something very bad has happened, function helper should have cleaned up
            panic!("Expected temporary value on the compiler local stack after compilation");
        }

        let index = compiler.chunk_mut().add_constant(Value::from(name.clone())); // TODO: how to avoid the clone here?
        define_global(compiler, index)
    }

    Ok(())
}

fn function(compiler: &mut Compiler, name: String, locals_offset: usize) -> UnitResult {
    compiler.begin_scope();

    compiler.consume(TokenType::LeftParen, "Expect '(' after function name.")?;

    let arity = function_parameters(compiler)?;
    compiler.func_stack.push(CompilerFunction {
        func: Function {
            arity,
            chunk: Chunk::new(),
            name,
        },
        locals_offset,
    });

    compiler.consume(
        TokenType::RightParen,
        "Expect ')' after function parameters.",
    )?;
    compiler.consume(TokenType::LeftBrace, "Expect '{' before function body.")?;

    block(compiler)?;
    emit_empty_return(compiler);

    compiler.end_scope();

    // pop the function that was just compiled and add it as a constant to the chunk of the parent function
    let func_value = compiler.func_stack.pop().unwrap().func.into();
    let index = compiler.chunk_mut().add_constant(func_value);
    compiler.emit_data_op(OpCode::Constant, index);

    Ok(())
}

fn emit_empty_return(compiler: &mut Compiler) {
    compiler.emit_op(OpCode::Nil);
    compiler.emit_op(OpCode::Return);
}

// consume parameter list and add each name to the current locals, doesn't emit any bytecode
fn function_parameters(compiler: &mut Compiler) -> BasicResult<u8> {
    if compiler.parser.current.token_type == TokenType::RightParen {
        Ok(0)
    } else {
        let mut arity = 0;
        loop {
            let identifier = compiler
                .consume(TokenType::Identifier, "Expect parameter name.")?
                .clone();
            // prevent duplicate parameter names on a function
            if compiler.identifier_in_current_scope(&identifier.lexeme) {
                return Err(build_error(
                    &format!("Duplicate parameter name ({}).", &identifier.lexeme),
                    identifier.line,
                ));
            } else if arity == u8::MAX {
                return Err(build_error(
                    "Function parameter list cannot exceed 255 entries",
                    identifier.line,
                ));
            }
            compiler.add_local(identifier);
            arity += 1;
            if !compiler.advance_if_match(TokenType::Comma)? {
                return Ok(arity);
            }
        }
    }
}

fn define_global(compiler: &mut Compiler, index: u8) {
    compiler.emit_data_op(OpCode::DefineGlobal, index);
}

fn statement(compiler: &mut Compiler) -> UnitResult {
    if compiler.advance_if_match(TokenType::LeftBrace)? {
        compiler.begin_scope();
        let result = block(compiler);
        compiler.end_scope();
        result
    } else if compiler.advance_if_match(TokenType::If)? {
        if_statement(compiler)
    } else if compiler.advance_if_match(TokenType::Return)? {
        return_statement(compiler)
    } else if compiler.advance_if_match(TokenType::While)? {
        while_statement(compiler)
    } else if compiler.advance_if_match(TokenType::For)? {
        for_statement(compiler)
    } else if compiler.advance_if_match(TokenType::Print)? {
        print_statement(compiler)
    } else {
        expression_statement(compiler)
    }
}

fn block(compiler: &mut Compiler) -> UnitResult {
    loop {
        if compiler.parser.current.token_type == TokenType::RightBrace
            || compiler.parser.current.token_type == TokenType::Eof
        {
            break;
        }
        declaration(compiler)?;
    }
    compiler.consume(TokenType::RightBrace, "Expect '}' after block.")?;
    Ok(())
}

fn if_statement(compiler: &mut Compiler) -> UnitResult {
    compiler.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
    expression(compiler)?;
    compiler.consume(TokenType::RightParen, "Expect ')' after condition.")?;

    let jump_over_then = emit_jump(compiler, OpCode::JumpIfFalse);

    // setup then branch
    compiler.emit_op(OpCode::Pop);
    statement(compiler)?;
    let jump_over_else = emit_jump(compiler, OpCode::Jump);

    patch_jump(compiler, jump_over_then);

    // setup else branch
    compiler.emit_op(OpCode::Pop);
    if compiler.advance_if_match(TokenType::Else)? {
        statement(compiler)?;
    }

    patch_jump(compiler, jump_over_else);

    Ok(())
}

fn return_statement(compiler: &mut Compiler) -> UnitResult {
    if compiler.advance_if_match(TokenType::Semicolon)? {
        emit_empty_return(compiler);
    } else {
        expression(compiler)?;
        compiler.consume(TokenType::Semicolon, "Expect ';' after return value.")?;
        compiler.emit_op(OpCode::Return);
    }
    Ok(())
}

fn while_statement(compiler: &mut Compiler) -> UnitResult {
    let loop_start = compiler.chunk().code.len();

    compiler.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
    expression(compiler)?;
    compiler.consume(TokenType::RightParen, "Expect ')' after condition.")?;

    let exit_jump = emit_jump(compiler, OpCode::JumpIfFalse);
    compiler.emit_op(OpCode::Pop);
    statement(compiler)?;

    emit_loop(compiler, loop_start);

    patch_jump(compiler, exit_jump);
    compiler.emit_op(OpCode::Pop);

    Ok(())
}

fn for_statement(compiler: &mut Compiler) -> UnitResult {
    compiler.begin_scope();
    compiler.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;

    // compile the optional initializer
    if compiler.advance_if_match(TokenType::Semicolon)? {
        // no initializer
    } else if compiler.advance_if_match(TokenType::Var)? {
        var_declaration(compiler)?;
    } else {
        expression_statement(compiler)?;
    }

    let condition_start = compiler.chunk().code.len();

    // compile the optional condition
    let exit_jump = if compiler.advance_if_match(TokenType::Semicolon)? {
        None
    } else {
        expression(compiler)?;
        compiler.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;
        let exit_jump = emit_jump(compiler, OpCode::JumpIfFalse);
        compiler.emit_op(OpCode::Pop);
        Some(exit_jump)
    };

    // compile the optional increment - this determines the start location for the loop instruction at the end of the body
    let loop_start = if compiler.advance_if_match(TokenType::RightParen)? {
        // no increment expression, so leave the loop start at the condition check
        condition_start
    } else {
        let body_jump = emit_jump(compiler, OpCode::Jump);
        let increment_start = compiler.chunk().code.len();
        expression(compiler)?;
        compiler.emit_op(OpCode::Pop);
        compiler.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;
        emit_loop(compiler, condition_start);
        patch_jump(compiler, body_jump);
        increment_start
    };

    // compile the loop body
    statement(compiler)?;
    emit_loop(compiler, loop_start);

    if let Some(exit_jump) = exit_jump {
        patch_jump(compiler, exit_jump);
        compiler.emit_op(OpCode::Pop);
    }

    compiler.end_scope();

    Ok(())
}

// emit jump instruction with dummy data for the jump distance, returns the location of the jump for later patching
fn emit_jump(compiler: &mut Compiler, opcode: OpCode) -> usize {
    compiler.emit_op(opcode);
    compiler.emit_data(0xff, 0xff);
    compiler.chunk().code.len() - 2
}

// patch the jump data at the given offset with the distance to the current instruction
fn patch_jump(compiler: &mut Compiler, offset: usize) {
    let jump_distance = compiler.chunk().code.len() - offset - 2;
    if jump_distance > (u16::MAX as usize) {
        panic!("Exceeded maximum jump size.");
    }
    compiler.chunk_mut().code[offset] = (jump_distance >> 8) as u8;
    compiler.chunk_mut().code[offset + 1] = jump_distance as u8;
}

// emit loop instruction with the jump distance computed back to the loop_start location
fn emit_loop(compiler: &mut Compiler, loop_start: usize) {
    compiler.emit_op(OpCode::Loop);
    let jump_distance = compiler.chunk().code.len() - loop_start + 2;
    if jump_distance > (u16::MAX as usize) {
        panic!("Loop body too large.");
    }
    compiler.emit_data((jump_distance >> 8) as u8, jump_distance as u8);
}

fn print_statement(compiler: &mut Compiler) -> UnitResult {
    expression(compiler)?;
    compiler.consume(TokenType::Semicolon, "Expect ';' after print statement.")?;
    compiler.emit_op(OpCode::Print);
    Ok(())
}

fn expression_statement(compiler: &mut Compiler) -> UnitResult {
    expression(compiler)?;
    compiler.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
    compiler.emit_op(OpCode::Pop);
    Ok(())
}

fn expression(compiler: &mut Compiler) -> UnitResult {
    parse_precedence(compiler, PREC_ASSIGNMENT)
}

fn parse_precedence(compiler: &mut Compiler, precedence: u32) -> UnitResult {
    compiler.parser.advance()?;
    let prefix_rule = get_rule(compiler.parser.previous.token_type).prefix;
    let prefix_rule = prefix_rule
        .ok_or_else(|| build_error("Expect expression.", compiler.parser.previous.line))?;

    // the target for assignment must be an expression with PREC_ASSIGNMENT or lower precedence
    let can_assign = precedence <= PREC_ASSIGNMENT;
    prefix_rule(compiler, can_assign)?;

    while precedence <= get_rule(compiler.parser.current.token_type).precedence {
        compiler.parser.advance()?;
        let infix_rule = get_rule(compiler.parser.previous.token_type).infix.unwrap();
        infix_rule(compiler, can_assign)?;
    }

    // detected assignment but the target expression didn't consume the equal token, improve the error message for this invalid assignment
    if can_assign && compiler.advance_if_match(TokenType::Equal)? {
        return Err(build_error(
            "Invalid assignment target.",
            compiler.parser.previous.line,
        ));
    }

    Ok(())
}

fn string(compiler: &mut Compiler, _can_assign: bool) -> UnitResult {
    let value = compiler.parser.previous.extract_string().clone().into();
    let address = compiler.chunk_mut().add_constant(value);
    compiler.emit_data_op(OpCode::Constant, address);
    Ok(())
}

fn number(compiler: &mut Compiler, _can_assign: bool) -> UnitResult {
    let value = compiler.parser.previous.extract_number().into();
    let address = compiler.chunk_mut().add_constant(value);
    compiler.emit_data_op(OpCode::Constant, address);
    Ok(())
}

fn literal(compiler: &mut Compiler, _can_assign: bool) -> UnitResult {
    match compiler.parser.previous.token_type {
        TokenType::Nil => compiler.emit_op(OpCode::Nil),
        TokenType::True => compiler.emit_op(OpCode::True),
        TokenType::False => compiler.emit_op(OpCode::False),
        _ => panic!(
            "Token at line {} is not a literal.",
            compiler.parser.previous.line
        ),
    }
    Ok(())
}

fn variable(compiler: &mut Compiler, can_assign: bool) -> UnitResult {
    let name = compiler.parser.previous.extract_name();
    let index: u8;
    // look first for a matching local variable
    let opcodes = if let Some(local_index) = compiler.compute_local_index(name) {
        // use the stack offset as the opcode data
        index = local_index;
        (OpCode::SetLocal, OpCode::GetLocal)
    } else {
        let name = name.clone();
        // save the global variable name as a string in the constant table
        index = compiler.chunk_mut().add_constant(Value::from(name));
        (OpCode::SetGlobal, OpCode::GetGlobal)
    };
    // determine whether loading value from or storing value into the variable
    if can_assign && compiler.advance_if_match(TokenType::Equal)? {
        expression(compiler)?;
        compiler.emit_data_op(opcodes.0, index);
    } else {
        compiler.emit_data_op(opcodes.1, index);
    }
    Ok(())
}

fn grouping(compiler: &mut Compiler, _can_assign: bool) -> UnitResult {
    expression(compiler)?;
    compiler.consume(TokenType::RightParen, "Expect ')' after expression.")?;
    Ok(())
}

fn call(compiler: &mut Compiler, _can_assign: bool) -> UnitResult {
    let arg_count = argument_list(compiler)?;
    compiler.emit_data_op(OpCode::Call, arg_count);
    Ok(())
}

fn argument_list(compiler: &mut Compiler) -> BasicResult<u8> {
    let mut arg_count = 0;
    if compiler.parser.current.token_type != TokenType::RightParen {
        loop {
            expression(compiler)?;
            if arg_count == 255 {
                return Err(build_error(
                    "Can't have more than 255 arguments",
                    compiler.parser.previous.line,
                ));
            }
            arg_count += 1;
            if !compiler.advance_if_match(TokenType::Comma)? {
                break;
            }
        }
    }
    compiler.consume(TokenType::RightParen, "Expect ')' after arguments.")?;
    Ok(arg_count)
}

fn unary(compiler: &mut Compiler, _can_assign: bool) -> UnitResult {
    let token_type = compiler.parser.previous.token_type;
    // compile operand
    parse_precedence(compiler, PREC_UNARY)?;
    match token_type {
        TokenType::Bang => compiler.emit_op(OpCode::Not),
        TokenType::Minus => compiler.emit_op(OpCode::Negate),
        _ => panic!("Unhandled unary token: {:?}", token_type),
    }
    Ok(())
}

fn binary(compiler: &mut Compiler, _can_assign: bool) -> UnitResult {
    let token_type = compiler.parser.previous.token_type;
    let rule = get_rule(token_type);
    parse_precedence(compiler, rule.precedence + 1)?;
    match token_type {
        TokenType::BangEqual => compiler.emit_ops(OpCode::Equal, OpCode::Not),
        TokenType::EqualEqual => compiler.emit_op(OpCode::Equal),
        TokenType::Greater => compiler.emit_op(OpCode::Greater),
        TokenType::GreaterEqual => compiler.emit_ops(OpCode::Less, OpCode::Not),
        TokenType::Less => compiler.emit_op(OpCode::Less),
        TokenType::LessEqual => compiler.emit_ops(OpCode::Greater, OpCode::Not),
        TokenType::Plus => compiler.emit_op(OpCode::Add),
        TokenType::Minus => compiler.emit_op(OpCode::Subtract),
        TokenType::Star => compiler.emit_op(OpCode::Multiply),
        TokenType::Slash => compiler.emit_op(OpCode::Divide),
        _ => panic!("Unhandled binary token: {:?}", token_type),
    }
    Ok(())
}

fn logic_and(compiler: &mut Compiler, _can_assign: bool) -> UnitResult {
    let jump = emit_jump(compiler, OpCode::JumpIfFalse);
    compiler.emit_op(OpCode::Pop);
    parse_precedence(compiler, PREC_AND)?;
    patch_jump(compiler, jump);
    Ok(())
}

fn logic_or(compiler: &mut Compiler, _can_assign: bool) -> UnitResult {
    let else_jump = emit_jump(compiler, OpCode::JumpIfFalse);
    let end_jump = emit_jump(compiler, OpCode::Jump);
    patch_jump(compiler, else_jump);
    compiler.emit_op(OpCode::Pop);
    parse_precedence(compiler, PREC_OR)?;
    patch_jump(compiler, end_jump);
    Ok(())
}

fn build_error(message: &str, line: u32) -> BasicError {
    BasicError::new(&format!("Compilation error at line {line}: {message}"))
}
