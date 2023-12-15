use std::mem;
use std::result;

use crate::error::{BasicError, BasicResult};
use crate::scanner::{Scanner, Token, TokenType};

use super::code::{self, Chunk, CompiledConstant, CompiledFunction, OpCode};

type UnitResult = BasicResult<()>;

const DEBUG_DUMP_CHUNK: bool = true;

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

struct Upvalue {
    is_local: bool,
    index: u8,
}

struct Local {
    name: String,
    depth: u32,
    is_captured: bool,
}

struct CompilerFunction {
    definition: CompiledFunction,
    locals: Vec<Local>,
    upvalues: Vec<Upvalue>,
    function_type: FunctionType,
}

struct CompilerClass {
    has_superclass: bool,
}

enum FunctionType {
    FreeFunction,
    ClassMethod,
    TypeInitializer,
}

struct Compiler {
    parser: Parser,
    func_stack: Vec<CompilerFunction>,
    class_stack: Vec<CompilerClass>,
    scope_depth: u32,
}

impl Compiler {
    fn new(parser: Parser) -> Compiler {
        // toplevel bytecode for a script will end up in this root function
        let root_func = CompiledFunction {
            arity: 0,
            upvalue_count: 0,
            chunk: Chunk::new(),
            name: "".to_string(),
        };
        Compiler {
            parser,
            func_stack: vec![CompilerFunction {
                definition: root_func,
                locals: Vec::new(),
                upvalues: Vec::new(),
                function_type: FunctionType::FreeFunction,
            }],
            class_stack: Vec::new(),
            scope_depth: 0,
        }
    }

    fn func(&self) -> &CompilerFunction {
        self.func_stack.last().unwrap()
    }

    fn func_mut(&mut self) -> &mut CompilerFunction {
        self.func_stack.last_mut().unwrap()
    }

    fn chunk(&self) -> &Chunk {
        &self.func_stack.last().unwrap().definition.chunk
    }

    fn chunk_mut(&mut self) -> &mut Chunk {
        &mut self.func_stack.last_mut().unwrap().definition.chunk
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

    fn end_scope_no_emit(&mut self) {
        self.scope_depth -= 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;

        while let Some(last) = self.func().locals.last() {
            if last.depth > self.scope_depth {
                let local = self.func_mut().locals.pop().unwrap();
                if local.is_captured {
                    self.emit_op(OpCode::CloseUpvalue);
                } else {
                    self.emit_op(OpCode::Pop);
                }
            } else {
                // locals stack is in depth order, so no need to proceed once an entry with lower depth is reached
                break;
            }
        }
    }

    fn add_local(&mut self, name: String) {
        if self.func().locals.len() >= u8::MAX.into() {
            panic!("Locals in scope limited to 256 entries at the moment.");
        }

        let entry = Local {
            name,
            depth: self.scope_depth,
            is_captured: false,
        };

        self.func_mut().locals.push(entry)
    }

    fn resolve_local(&self, name: &str) -> Option<u8> {
        Compiler::resolve_function_local(self.func(), name)
    }

    fn resolve_function_local(function: &CompilerFunction, name: &str) -> Option<u8> {
        if let Some((match_index, _)) = function
            .locals
            .iter()
            .enumerate()
            .rev()
            .find(|entry| entry.1.name == name)
        {
            Some(match_index as u8)
        } else {
            None
        }
    }

    // find a local from an enclosing function and automatically add upvalue entries for all functions in between
    fn resolve_upvalue(&mut self, name: &str) -> Option<u8> {
        if self.func_stack.len() <= 1 {
            // no enclosing function to search
            return None;
        }

        // second to last entry on the stack is the nearest enclosing function (first ancestor)
        let ancestors = &self.func_stack[0..(self.func_stack.len() - 1)];
        let mut upvalue_location: Option<(usize, u8)> = None;
        // search for a matching local starting from innermost to outermost enclosing functions (stack top to stack bottom)
        for (function_index, function) in ancestors.iter().enumerate().rev() {
            if let Some(local_index) = Compiler::resolve_function_local(function, name) {
                upvalue_location = Some((function_index, local_index));
            }
        }

        if let Some((function_index, local_index)) = upvalue_location {
            // mark the local itself as captured for special upvalue handling when it goes out of lexical scope
            self.func_stack[function_index].locals[local_index as usize].is_captured = true;
            // work from the first capture location to the current function, adding an upvalue entry for each along the way
            let mut parent_upvalue_index = local_index;
            let mut is_local = true;
            let func_stack_length = self.func_stack.len();
            for function in self.func_stack[(function_index + 1)..func_stack_length].iter_mut() {
                parent_upvalue_index =
                    Compiler::add_upvalue(function, is_local, parent_upvalue_index);
                // the first capture points to an enclosing function's local but all upvalues down the line will not
                is_local = false;
            }
            Some(parent_upvalue_index)
        } else {
            None
        }
    }

    fn add_upvalue(function: &mut CompilerFunction, is_local: bool, index: u8) -> u8 {
        if function.upvalues.len() >= u8::MAX as usize {
            panic!(
                "Exceeded maximum upvalue count while processing function {}",
                function.definition.name
            )
        }

        if let Some(matching_index) = function
            .upvalues
            .iter()
            .position(|upvalue| upvalue.is_local == is_local && upvalue.index == index)
        {
            matching_index as u8
        } else {
            function.upvalues.push(Upvalue { is_local, index });
            function.definition.upvalue_count = function.upvalues.len() as u8;
            (function.upvalues.len() - 1) as u8
        }
    }

    fn identifier_in_current_scope(&self, name: &str) -> bool {
        for local in self.func().locals.iter().rev() {
            // locals are always added in order of scope depth, so we can bail as soon as we hit one at a lower depth
            if local.depth != self.scope_depth {
                return false;
            } else if local.name == name {
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
    (TokenType::Dot,            ParseRule { prefix: None,           infix: Some(dot),       precedence: PREC_CALL }),
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
    (TokenType::Super,          ParseRule { prefix: Some(super_),   infix: None,            precedence: PREC_NONE }),
    (TokenType::This,           ParseRule { prefix: Some(this),     infix: None,            precedence: PREC_NONE }),
    (TokenType::True,           ParseRule { prefix: Some(literal),  infix: None,            precedence: PREC_NONE }),
    (TokenType::Var,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::While,          ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE }),
    (TokenType::Eof,            ParseRule { prefix: None,           infix: None,            precedence: PREC_NONE })];

fn get_rule(token_type: TokenType) -> &'static ParseRule {
    &PARSE_TABLE[token_type as usize].1
}

pub fn compile(source: &str) -> result::Result<CompiledFunction, BasicError> {
    let parser = Parser::new(Scanner::new(source))?;
    let mut compiler = Compiler::new(parser);

    while compiler.parser.current.token_type != TokenType::Eof {
        declaration(&mut compiler)?;
    }

    emit_empty_return(&mut compiler);

    if DEBUG_DUMP_CHUNK {
        code::disassemble_chunk(&compiler.func_stack[0].definition.chunk, "root");
    }

    Ok(compiler.func_stack.remove(0).definition)
}

fn declaration(compiler: &mut Compiler) -> UnitResult {
    if compiler.advance_if_match(TokenType::Var)? {
        var_declaration(compiler)
    } else if compiler.advance_if_match(TokenType::Fun)? {
        fun_declaration(compiler)
    } else if compiler.advance_if_match(TokenType::Class)? {
        class_declaration(compiler)
    } else {
        statement(compiler)
    }
}

fn var_declaration(compiler: &mut Compiler) -> UnitResult {
    let identifier = compiler
        .consume(TokenType::Identifier, "Expect variable name.")?
        .extract_name()
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
        let index = compiler.chunk_mut().add_constant(CompiledConstant::from(identifier));
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
    function(
        compiler,
        identifier.extract_name().clone(),
        FunctionType::FreeFunction,
    )?;
    define_variable_no_replace(compiler, identifier)?;
    Ok(())
}

fn define_variable_no_replace(compiler: &mut Compiler, identifier: Token) -> UnitResult {
    if compiler.scope_depth == 0 {
        let index = compiler
            .chunk_mut()
            .add_constant(CompiledConstant::from(identifier.into_name()));
        define_global(compiler, index)
    } else {
        // unlike variables, functions and classes are not allowed to replace existing names within the same scope
        if compiler.identifier_in_current_scope(identifier.extract_name()) {
            return Err(build_error(
                &format!(
                    "Already a variable in scope matching the function or class name ({}).",
                    identifier.extract_name()
                ),
                identifier.line,
            ));
        } else {
            compiler.add_local(identifier.into_name());
        }
    }
    Ok(())
}

fn function(compiler: &mut Compiler, name: String, function_type: FunctionType) -> UnitResult {
    compiler.consume(TokenType::LeftParen, "Expect '(' after function name.")?;

    compiler.func_stack.push(CompilerFunction {
        definition: CompiledFunction {
            arity: 0,
            upvalue_count: 0,
            chunk: Chunk::new(),
            name: name.clone(),
        },
        locals: Vec::new(),
        upvalues: Vec::new(),
        function_type,
    });

    compiler.begin_scope();

    match compiler.func().function_type {
        // the free function calling convention has the callee in local slot zero
        FunctionType::FreeFunction => compiler.add_local(name),
        // the class method and type init calling conventions have the object instance in local slot zero
        FunctionType::ClassMethod | FunctionType::TypeInitializer => {
            compiler.add_local(code::NAME_FOR_SELF.to_owned())
        }
    }

    let arity = function_parameters(compiler)?;
    compiler.func_mut().definition.arity = arity;

    compiler.consume(
        TokenType::RightParen,
        "Expect ')' after function parameters.",
    )?;
    compiler.consume(TokenType::LeftBrace, "Expect '{' before function body.")?;

    block(compiler)?;
    emit_empty_return(compiler);

    // TODO: figure out a cleaner way to do this, don't need to clean up function locals (return does that) but do need to reset scope depth
    compiler.end_scope_no_emit();

    // pop the function that was just compiled and add it as a constant to the chunk of the parent function
    let compiled_func = compiler.func_stack.pop().unwrap();
    let constant_index = compiler
        .chunk_mut()
        .add_constant(CompiledConstant::from(compiled_func.definition));
    compiler.emit_data_op(OpCode::Closure, constant_index);
    for upvalue in compiled_func.upvalues {
        let is_local = if upvalue.is_local { 1 } else { 0 };
        compiler.emit_data(is_local, upvalue.index);
    }

    Ok(())
}

fn emit_empty_return(compiler: &mut Compiler) {
    if matches!(compiler.func().function_type, FunctionType::TypeInitializer) {
        // type initializer must always return the bound method instance to the caller, local slot zero with our calling convention
        compiler.emit_data_op(OpCode::GetLocal, 0)
    } else {
        compiler.emit_op(OpCode::Nil);
    }
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
            if compiler.identifier_in_current_scope(identifier.extract_name()) {
                return Err(build_error(
                    &format!("Duplicate parameter name ({}).", identifier.extract_name()),
                    identifier.line,
                ));
            } else if arity == u8::MAX {
                return Err(build_error(
                    "Function parameter list cannot exceed 255 entries",
                    identifier.line,
                ));
            }
            compiler.add_local(identifier.into_name());
            arity += 1;
            if !compiler.advance_if_match(TokenType::Comma)? {
                return Ok(arity);
            }
        }
    }
}

fn class_declaration(compiler: &mut Compiler) -> UnitResult {
    let identifier = compiler
        .consume(TokenType::Identifier, "Expect class name.")?
        .clone();
    let class_name_constant = compiler
        .chunk_mut()
        .add_constant(CompiledConstant::from(identifier.extract_name().clone()));
    compiler.emit_data_op(OpCode::Class, class_name_constant);

    // NOTE: for global vars right now, this will add a duplicate string entry in the constant table with the class name
    define_variable_no_replace(compiler, identifier.clone())?;

    let has_superclass = compiler.advance_if_match(TokenType::Less)?;
    if has_superclass {
        let super_name = compiler
            .consume(TokenType::Identifier, "Expect superclass name.")?
            .extract_name()
            .clone();
        let class_name = identifier.extract_name();
        if super_name == *class_name {
            return Err(build_error(
                "Class cannot inherit from itself.",
                compiler.parser.previous.line,
            ));
        }
        // emit code to load the superclass and current class values onto the stack and execute inherit
        load_named_variable(compiler, &super_name);
        load_named_variable(compiler, class_name);
        compiler.emit_op(OpCode::Inherit);
        // inherit leaves superclass on the stack, references to "super" will use this synthetic local value
        compiler.begin_scope();
        compiler.add_local(code::NAME_FOR_SUPERCLASS.to_owned());
    }

    compiler.class_stack.push(CompilerClass { has_superclass });
    compiler.consume(TokenType::LeftBrace, "Expect '{' before class body.")?;

    // load the class onto the top of the stack for following method assignment instructions
    load_named_variable(compiler, &identifier.into_name());
    while compiler.parser.current.token_type != TokenType::RightBrace
        && compiler.parser.current.token_type != TokenType::Eof
    {
        method(compiler)?;
    }
    compiler.emit_op(OpCode::Pop);

    compiler.consume(TokenType::RightBrace, "Expect '}' after class body.")?;
    let finished_class = compiler.class_stack.pop().unwrap();
    if finished_class.has_superclass {
        compiler.end_scope();
    }

    Ok(())
}

fn method(compiler: &mut Compiler) -> UnitResult {
    let method_name = compiler
        .consume(TokenType::Identifier, "Expect class name.")?
        .extract_name()
        .clone();

    // special handling for return statements within type initializer (init) method
    let function_type = if method_name == code::TYPE_INITIALIZER_METHOD {
        FunctionType::TypeInitializer
    } else {
        FunctionType::ClassMethod
    };
    function(compiler, method_name.clone(), function_type)?;

    let name_constant = compiler.chunk_mut().add_constant(CompiledConstant::from(method_name));
    compiler.emit_data_op(OpCode::Method, name_constant);
    Ok(())
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
        if matches!(compiler.func().function_type, FunctionType::TypeInitializer) {
            return Err(build_error(
                "Can't return a value from an initializer.",
                compiler.parser.previous.line,
            ));
        }
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
    let value = CompiledConstant::from(compiler.parser.previous.extract_string().clone());
    let address = compiler.chunk_mut().add_constant(value);
    compiler.emit_data_op(OpCode::Constant, address);
    Ok(())
}

fn number(compiler: &mut Compiler, _can_assign: bool) -> UnitResult {
    let value = CompiledConstant::from(compiler.parser.previous.extract_number());
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

fn super_(compiler: &mut Compiler, _can_assign: bool) -> UnitResult {
    if compiler.class_stack.is_empty() {
        return Err(build_error(
            "Can't use 'super' outside of a class.",
            compiler.parser.previous.line,
        ));
    } else if !compiler.class_stack.last().unwrap().has_superclass {
        return Err(build_error(
            "Can't use 'super' in a class with no superclass.",
            compiler.parser.previous.line,
        ));
    }
    compiler.consume(TokenType::Dot, "Expect '.' after 'super'.")?;
    let method_name = compiler.consume(TokenType::Identifier, "Expect superclass method name.")?.extract_name().clone();
    let method_name = compiler.chunk_mut().add_constant(CompiledConstant::from(method_name));
    load_named_variable(compiler, code::NAME_FOR_SELF);
    if compiler.advance_if_match(TokenType::LeftParen)? {
        // optimization for direct invocation
        let arg_count = argument_list(compiler)?;
        load_named_variable(compiler, code::NAME_FOR_SUPERCLASS);
        compiler.emit_op(OpCode::SuperInvoke);
        compiler.emit_data(method_name, arg_count);
    } else {
        load_named_variable(compiler, code::NAME_FOR_SUPERCLASS);
        compiler.emit_data_op(OpCode::GetSuper, method_name);
    }
    Ok(())
}

// handled like a special variable name that cannot be assigned to and is only valid inside of a method definition
fn this(compiler: &mut Compiler, _can_assign: bool) -> UnitResult {
    if compiler.class_stack.is_empty() {
        return Err(build_error(
            "Can't use 'this' outside of a class.",
            compiler.parser.previous.line,
        ));
    }
    load_named_variable(compiler, code::NAME_FOR_SELF);
    Ok(())
}

fn variable(compiler: &mut Compiler, can_assign: bool) -> UnitResult {
    let name = compiler.parser.previous.extract_name().clone();
    let index: u8;
    // look first for a matching local variable
    let opcodes = if let Some(local_index) = compiler.resolve_local(&name) {
        // use the stack offset as the opcode data
        index = local_index;
        (OpCode::SetLocal, OpCode::GetLocal)
    } else if let Some(upvalue_index) = compiler.resolve_upvalue(&name) {
        index = upvalue_index;
        (OpCode::SetUpvalue, OpCode::GetUpvalue)
    } else {
        // save the global variable name as a string in the constant table
        index = compiler.chunk_mut().add_constant(CompiledConstant::from(name));
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

// emit instruction to load the given variable name from local scope, upvalue, or global scope
fn load_named_variable(compiler: &mut Compiler, name: &str) {
    let (opcode, index) = if let Some(local_index) = compiler.resolve_local(name) {
        (OpCode::GetLocal, local_index)
    } else if let Some(upvalue_index) = compiler.resolve_upvalue(name) {
        (OpCode::GetUpvalue, upvalue_index)
    } else {
        (
            OpCode::GetGlobal,
            compiler
                .chunk_mut()
                .add_constant(CompiledConstant::from(name.to_string())),
        )
    };
    compiler.emit_data_op(opcode, index);
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

fn dot(compiler: &mut Compiler, can_assign: bool) -> UnitResult {
    let identifier = compiler.consume(TokenType::Identifier, "Expect property name after '.'.")?;
    let property_name = identifier.extract_name().clone();
    let property_name = compiler
        .chunk_mut()
        .add_constant(CompiledConstant::from(property_name));

    if can_assign && compiler.advance_if_match(TokenType::Equal)? {
        expression(compiler)?;
        compiler.emit_data_op(OpCode::SetProperty, property_name);
    } else if compiler.advance_if_match(TokenType::LeftParen)? {
        // detect call immediately after access and emit optimized form that avoids creating a full method binding
        let arg_count = argument_list(compiler)?;
        compiler.emit_op(OpCode::Invoke);
        compiler.emit_data(property_name, arg_count);
    } else {
        compiler.emit_data_op(OpCode::GetProperty, property_name);
    }
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
