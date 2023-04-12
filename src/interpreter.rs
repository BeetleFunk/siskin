use crate::error::BasicError;
use crate::error::GenericResult;
use crate::expr::Expr;
use crate::expr::LiteralValue;
use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::stmt::Stmt;

use std::collections::HashMap;
use std::io::Write;

type UnitResult = GenericResult<()>;
type ValueResult = GenericResult<LiteralValue>;

pub struct Environment<'a> {
    stack: Vec<HashMap<String, LiteralValue>>,
    output_writer: &'a mut dyn Write,
}

impl<'a> Environment<'a> {
    pub fn new(output_writer: &'a mut dyn Write) -> Environment<'a> {
        // initialize the global environment map as the first entry on the stack
        Environment {
            stack: vec![HashMap::new()],
            output_writer,
        }
    }

    fn push(&mut self) {
        self.stack.push(HashMap::new());
    }

    fn pop(&mut self) {
        self.stack.pop();
    }

    fn define(&mut self, name: String, value: LiteralValue) {
        let frame = self
            .stack
            .last_mut()
            .expect("Missing global scope frame from Environment stack."); // panic here because this would indicate a bug in the interpreter
        frame.insert(name, value);
    }

    fn assign(&mut self, name: String, value: LiteralValue) -> UnitResult {
        if let Some(frame) = self.stack.iter_mut().rev().find(|x| x.contains_key(&name)) {
            frame.insert(name, value);
            Ok(())
        } else {
            Err(Box::new(BasicError::new(&format!(
                "Undefined variable ({name})"
            ))))
        }
    }

    fn get(&self, name: &str) -> Option<&LiteralValue> {
        for frame in self.stack.iter().rev() {
            if let Some(value) = frame.get(name) {
                return Some(value);
            }
        }
        None
    }
}

pub fn execute(statements: &Vec<Stmt>, env: &mut Environment) -> UnitResult {
    for statement in statements {
        execute_statement(statement, env)?;
    }
    Ok(())
}

fn execute_statement(statement: &Stmt, env: &mut Environment) -> UnitResult {
    match statement {
        Stmt::Block { statements } => block_statement(statements, env),
        Stmt::Expression { expression } => expression_statement(expression, env),
        Stmt::If { condition, then_branch, else_branch } => if_statement(condition, then_branch, else_branch, env),
        Stmt::Print { expression } => print_statement(expression, env),
        Stmt::Var { name, initializer } => var_statement(name, initializer, env),
    }
}

fn block_statement(statements: &Vec<Stmt>, env: &mut Environment) -> UnitResult {
    env.push();
    for statement in statements {
        let result = execute_statement(statement, env);

        // make sure to restore the stack even after an error. TODO: cleaner way to accomplish this?
        if result.is_err() {
            env.pop();
            return result;
        }
    }
    env.pop();
    Ok(())
}

fn expression_statement(expression: &Expr, env: &mut Environment) -> UnitResult {
    evaluate(expression, env)?;
    Ok(())
}

fn if_statement(condition: &Expr, then_branch: &Box<Stmt>, else_branch: &Option<Box<Stmt>>, env: &mut Environment) -> UnitResult {
    if is_truthy(&evaluate(condition, env)?) {
        execute_statement(then_branch, env)
    } else if let Some(else_statement) = else_branch {
        execute_statement(else_statement, env)
    } else {
        Ok(())
    }
}

fn print_statement(expression: &Expr, env: &mut Environment) -> UnitResult {
    let result = evaluate(expression, env)?;
    writeln!(env.output_writer, "{}", result.to_string())
        .expect("Writing to program output should always succeed.");
    Ok(())
}

fn var_statement(name: &Token, initializer: &Expr, env: &mut Environment) -> UnitResult {
    let result = evaluate(initializer, env)?;
    env.define(name.lexeme.clone(), result);
    Ok(())
}

fn evaluate(expression: &Expr, env: &mut Environment) -> ValueResult {
    match expression {
        Expr::Assign { name, value } => evaluate_assign(name, value, env),
        Expr::Binary {
            left,
            operator,
            right,
        } => evaluate_binary(left, operator, right, env),
        Expr::Grouping { expression } => evaluate_grouping(expression, env),
        Expr::Literal { value } => evaluate_literal(value),
        Expr::Unary { operator, right } => evaluate_unary(operator, right, env),
        Expr::Variable { name } => evaluate_variable(name, env),
    }
}

fn evaluate_assign(name: &Token, value: &Box<Expr>, env: &mut Environment) -> ValueResult {
    let result = evaluate(value, env)?;
    env.assign(name.lexeme.clone(), result.clone())
        .or_else(|e| Err(build_error(&e.to_string(), name.line)))?;
    Ok(result)
}

fn evaluate_binary(
    left: &Box<Expr>,
    operator: &Token,
    right: &Box<Expr>,
    env: &mut Environment,
) -> ValueResult {
    let left_evaluated = evaluate(left, env)?;
    let right_evaluated = evaluate(right, env)?;

    let evaluated = if is_numeric_binary_operation(&operator.token_type) {
        let left_number = extract_number(&left_evaluated).ok_or_else(|| {
            build_error(
                "Cannot perform numeric operation on non-numeric value.",
                operator.line,
            )
        })?;
        let right_number = extract_number(&right_evaluated).ok_or_else(|| {
            build_error(
                "Cannot perform numeric operation on non-numeric value.",
                operator.line,
            )
        })?;

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
            _ => panic!(
                "Unhandled binary numeric operation type: {:?}",
                operator.token_type
            ),
        }
    } else {
        match operator.token_type {
            TokenType::EqualEqual => LiteralValue::Boolean(left_evaluated == right_evaluated),
            TokenType::BangEqual => LiteralValue::Boolean(left_evaluated != right_evaluated),
            // unhandled case here indicates a bug in the parser or interpreter
            _ => panic!(
                "Unhandled binary non-numeric operation type: {:?}",
                operator.token_type
            ),
        }
    };
    Ok(evaluated)
}

fn evaluate_grouping(expression: &Box<Expr>, env: &mut Environment) -> ValueResult {
    evaluate(expression, env)
}

fn evaluate_literal(value: &LiteralValue) -> ValueResult {
    Ok(value.clone())
}

fn evaluate_unary(operator: &Token, right: &Box<Expr>, env: &mut Environment) -> ValueResult {
    let operand = evaluate(right, env)?;
    let evaluated = match operator.token_type {
        TokenType::Bang => LiteralValue::Boolean(!is_truthy(&operand)),
        TokenType::Minus => {
            let original = extract_number(&operand).ok_or_else(|| {
                build_error(
                    "Cannot perform negation operation on non-numeric value.",
                    operator.line,
                )
            })?;
            LiteralValue::Number(-original)
        }
        // unhandled case here indicates a bug in the parser or interpreter
        _ => panic!(
            "Unary expression not implemented in interpreter: {:?}",
            operator
        ),
    };
    Ok(evaluated)
}

fn evaluate_variable(name: &Token, env: &Environment) -> ValueResult {
    if let Some(value) = env.get(&name.lexeme) {
        Ok(value.clone())
    } else {
        Err(Box::new(build_error(
            &format!("Variable ({}) not found.", name.lexeme),
            name.line,
        )))
    }
}

fn extract_number(value: &LiteralValue) -> Option<f64> {
    match value {
        LiteralValue::Number(result) => Some(*result),
        _ => None,
    }
}

fn is_truthy(value: &LiteralValue) -> bool {
    match value {
        LiteralValue::Nil => false,
        LiteralValue::Boolean(value) => *value,
        _ => true,
    }
}

fn is_numeric_binary_operation(operator: &TokenType) -> bool {
    match operator {
        TokenType::Minus
        | TokenType::Slash
        | TokenType::Star
        | TokenType::Plus
        | TokenType::Greater
        | TokenType::GreaterEqual
        | TokenType::Less
        | TokenType::LessEqual => true,
        _ => false,
    }
}

fn build_error(message: &str, line: u32) -> BasicError {
    BasicError::new(&format!("Interpret error at line {line}: {message}"))
}
