use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::result;

use crate::error::BasicError;
use crate::scanner::Token;
use crate::scanner::TokenType;

use super::environment::Environment;
use super::environment::Reference;
use super::environment::SiskinFunction;
use super::environment::SiskinValue;
use super::expr::Expr;
use super::expr::LiteralValue;
use super::resolver::resolve_function_captures;
use super::stmt::Stmt;

type ValueResult = result::Result<SiskinValue, BasicError>;
// Error result can be either an early return value or an interpreter error
type StatementResult = result::Result<(), ReturnValueOrError>;

#[derive(Debug)]
pub struct ReturnValueOrError {
    result: ValueResult,
}

impl ReturnValueOrError {
    pub fn new(value: SiskinValue) -> ReturnValueOrError {
        ReturnValueOrError { result: Ok(value) }
    }
}

impl fmt::Display for ReturnValueOrError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.result {
            Ok(value) => write!(f, "{}", value),
            Err(error) => write!(f, "{}", error),
        }
    }
}

impl From<BasicError> for ReturnValueOrError {
    fn from(error: BasicError) -> Self {
        ReturnValueOrError { result: Err(error) }
    }
}

impl Error for ReturnValueOrError {}

pub fn execute(statements: &[Stmt], env: &mut Environment) -> StatementResult {
    for statement in statements {
        execute_statement(statement, env)?;
    }
    Ok(())
}

fn execute_statement(statement: &Stmt, env: &mut Environment) -> StatementResult {
    match statement {
        Stmt::Block { statements } => block_statement(statements, env),
        Stmt::Expression { expression } => expression_statement(expression, env),
        Stmt::Function { name, params, body } => function_statement(name, params, body, env),
        Stmt::If {
            condition,
            then_branch,
            else_branch,
        } => if_statement(condition, then_branch, else_branch, env),
        Stmt::Print { expression } => print_statement(expression, env),
        Stmt::Return { line, expression } => return_statement(line, expression, env),
        Stmt::Var { name, initializer } => var_statement(name, initializer, env),
        Stmt::While { condition, body } => while_statement(condition, body, env),
    }
}

fn block_statement(statements: &[Stmt], env: &mut Environment) -> StatementResult {
    env.push();
    for statement in statements {
        let result = execute_statement(statement, env);

        // make sure to restore the stack even after an error.
        if result.is_err() {
            env.pop();
            return result;
        }
    }
    env.pop();
    Ok(())
}

fn expression_statement(expression: &Expr, env: &mut Environment) -> StatementResult {
    evaluate(expression, env)?;
    Ok(())
}

fn function_statement(name: &Token, params: &[Token], body: &Stmt, env: &mut Environment) -> StatementResult {
    let captured_names = resolve_function_captures(params, body)?;
    let mut captured_vars: HashMap<String, Reference> = HashMap::new();
    for capture in captured_names {
        if let Some(reference) = env.get_reference(&capture.lexeme) {
            captured_vars.insert(capture.lexeme, reference);
        } else if capture.lexeme != name.lexeme {
            // detect recursion, and avoid the error in that case
            return Err(ReturnValueOrError::from(build_error(
                &format!(
                    "Could not locate capture variable ({}) in function ({}).",
                    capture.lexeme, name.lexeme
                ),
                name.line,
            )));
        }

        //writeln!(env.output_writer, "Captured var name: {}", capture.lexeme).expect("Writing to program output should always succeed.");
    }

    let function = SiskinFunction {
        name: name.clone(),
        params: Vec::from(params),
        body: body.clone(),
        captured_vars,
    };
    env.define(name.lexeme.clone(), SiskinValue::Function(function));
    Ok(())
}

fn if_statement(
    condition: &Expr,
    then_branch: &Stmt,
    else_branch: &Option<Box<Stmt>>,
    env: &mut Environment,
) -> StatementResult {
    if is_truthy(&evaluate(condition, env)?) {
        execute_statement(then_branch, env)
    } else if let Some(else_statement) = else_branch {
        execute_statement(else_statement, env)
    } else {
        Ok(())
    }
}

fn print_statement(expression: &Expr, env: &mut Environment) -> StatementResult {
    let result = evaluate(expression, env)?;
    writeln!(env.output_writer, "{}", result).expect("Writing to program output should always succeed.");
    Ok(())
}

// Uses custom error type to pass early return values up the evaluation tree
fn return_statement(_line: &u32, expression: &Expr, env: &mut Environment) -> StatementResult {
    let return_value = evaluate(expression, env)?;
    Err(ReturnValueOrError::new(return_value))
}

fn var_statement(name: &Token, initializer: &Expr, env: &mut Environment) -> StatementResult {
    let result = evaluate(initializer, env)?;
    env.define(name.lexeme.clone(), result);
    Ok(())
}

fn while_statement(condition: &Expr, body: &Stmt, env: &mut Environment) -> StatementResult {
    while is_truthy(&evaluate(condition, env)?) {
        execute_statement(body, env)?;
    }
    Ok(())
}

fn evaluate(expression: &Expr, env: &mut Environment) -> ValueResult {
    match expression {
        Expr::Assign { name, value } => evaluate_assign(name, value, env),
        Expr::Binary { left, operator, right } => evaluate_binary(left, operator, right, env),
        Expr::Call {
            callee,
            paren,
            arguments,
        } => evaluate_call(callee, paren, arguments, env),
        Expr::Grouping { expression } => evaluate_grouping(expression, env),
        Expr::Literal { value } => evaluate_literal(value),
        Expr::Logical { left, operator, right } => evaluate_logical(left, operator, right, env),
        Expr::Unary { operator, right } => evaluate_unary(operator, right, env),
        Expr::Variable { name } => evaluate_variable(name, env),
    }
}

fn evaluate_assign(name: &Token, value: &Expr, env: &mut Environment) -> ValueResult {
    let result = evaluate(value, env)?;
    env.assign(name.lexeme.clone(), result.clone())
        .map_err(|e| build_error(&e.to_string(), name.line))?;
    Ok(result)
}

fn evaluate_binary(left: &Expr, operator: &Token, right: &Expr, env: &mut Environment) -> ValueResult {
    let left_evaluated = evaluate(left, env)?;
    let right_evaluated = evaluate(right, env)?;

    let evaluated = if is_numeric_binary_operation(&operator.token_type) {
        let left_number = extract_number(&left_evaluated)
            .ok_or_else(|| build_error("Cannot perform numeric operation on non-numeric value.", operator.line))?;
        let right_number = extract_number(&right_evaluated)
            .ok_or_else(|| build_error("Cannot perform numeric operation on non-numeric value.", operator.line))?;

        match operator.token_type {
            TokenType::Minus => LiteralValue::Number(left_number - right_number),
            TokenType::Slash => LiteralValue::Number(left_number / right_number),
            TokenType::Star => LiteralValue::Number(left_number * right_number),
            TokenType::Plus => LiteralValue::Number(left_number + right_number),
            TokenType::Greater => LiteralValue::Boolean(left_number > right_number),
            TokenType::GreaterEqual => LiteralValue::Boolean(left_number >= right_number),
            TokenType::Less => LiteralValue::Boolean(left_number < right_number),
            TokenType::LessEqual => LiteralValue::Boolean(left_number <= right_number),
            // unhandled case here indicates a bug in the parser or interpreter
            _ => panic!("Unhandled binary numeric operation type: {:?}", operator.token_type),
        }
    } else {
        match operator.token_type {
            TokenType::EqualEqual => LiteralValue::Boolean(left_evaluated == right_evaluated),
            TokenType::BangEqual => LiteralValue::Boolean(left_evaluated != right_evaluated),
            // unhandled case here indicates a bug in the parser or interpreter
            _ => panic!("Unhandled binary non-numeric operation type: {:?}", operator.token_type),
        }
    };
    Ok(SiskinValue::from(evaluated))
}

fn evaluate_call(callee: &Expr, paren: &Token, arguments: &[Expr], env: &mut Environment) -> ValueResult {
    let callee = evaluate(callee, env)?;
    let evaluated_args: Vec<ValueResult> = arguments.iter().map(|arg| evaluate(arg, env)).collect();
    let mut unwrapped_args = Vec::new();
    // WORK IN PROGRESS: if errors occurred during evaluation just return the first one
    for result in evaluated_args {
        unwrapped_args.push(result?);
    }

    let return_value = run_function(callee, unwrapped_args, paren.line, env)?;
    Ok(return_value)
}

fn run_function(
    function_handle: SiskinValue,
    arguments: Vec<SiskinValue>,
    line: u32,
    env: &mut Environment,
) -> ValueResult {
    match function_handle {
        SiskinValue::FunctionHandle(_) => {
            let function = env.get_function(&function_handle);
            env.push();
            for (i, item) in function.params.iter().enumerate() {
                env.define(item.lexeme.clone(), arguments[i].clone());
            }

            // make captured variables available in current scope
            for capture in function.captured_vars {
                env.capture_reference(capture.0, capture.1);
            }

            let return_value = execute_statement(&function.body, env);
            env.pop();

            if let Err(value_or_error) = return_value {
                let value = value_or_error.result?;
                Ok(value)
            } else {
                Ok(SiskinValue::from(LiteralValue::Nil))
            }
        }
        _ => Err(build_error("Cannot call non-function expression.", line)),
    }
}

fn evaluate_grouping(expression: &Expr, env: &mut Environment) -> ValueResult {
    evaluate(expression, env)
}

fn evaluate_literal(value: &LiteralValue) -> ValueResult {
    Ok(SiskinValue::from(value.clone()))
}

fn evaluate_logical(left: &Expr, operator: &Token, right: &Expr, env: &mut Environment) -> ValueResult {
    let left_evaluated = evaluate(left, env)?;

    // short circuit if possible
    if operator.token_type == TokenType::Or {
        if is_truthy(&left_evaluated) {
            return Ok(left_evaluated);
        }
    } else if operator.token_type == TokenType::And {
        if !is_truthy(&left_evaluated) {
            return Ok(left_evaluated);
        }
    } else {
        return Err(build_error("Unhandled logical operator.", operator.line));
    }

    let right_evaluated = evaluate(right, env)?;
    Ok(right_evaluated)
}

fn evaluate_unary(operator: &Token, right: &Expr, env: &mut Environment) -> ValueResult {
    let operand = evaluate(right, env)?;
    let evaluated = match operator.token_type {
        TokenType::Bang => LiteralValue::Boolean(!is_truthy(&operand)),
        TokenType::Minus => {
            let original = extract_number(&operand)
                .ok_or_else(|| build_error("Cannot perform negation operation on non-numeric value.", operator.line))?;
            LiteralValue::Number(-original)
        }
        // unhandled case here indicates a bug in the parser or interpreter
        _ => panic!("Unary expression not implemented in interpreter: {:?}", operator),
    };
    Ok(SiskinValue::from(evaluated))
}

fn evaluate_variable(name: &Token, env: &Environment) -> ValueResult {
    if let Some(value) = env.get(&name.lexeme) {
        Ok(value)
    } else {
        Err(build_error(
            &format!("Variable ({}) not found.", name.lexeme),
            name.line,
        ))
    }
}

fn extract_number(value: &SiskinValue) -> Option<f64> {
    match value {
        SiskinValue::Literal(LiteralValue::Number(result)) => Some(*result),
        _ => None,
    }
}

fn is_truthy(value: &SiskinValue) -> bool {
    match value {
        SiskinValue::Literal(LiteralValue::Nil) => false,
        SiskinValue::Literal(LiteralValue::Boolean(value)) => *value,
        _ => true,
    }
}

fn is_numeric_binary_operation(operator: &TokenType) -> bool {
    matches!(
        operator,
        TokenType::Minus
            | TokenType::Slash
            | TokenType::Star
            | TokenType::Plus
            | TokenType::Greater
            | TokenType::GreaterEqual
            | TokenType::Less
            | TokenType::LessEqual
    )
}

fn build_error(message: &str, line: u32) -> BasicError {
    BasicError::new(&format!("Interpret error at line {line}: {message}"))
}
