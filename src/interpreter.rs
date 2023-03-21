use crate::expr::Expr;
use crate::expr::LiteralValue;
use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::stmt::Stmt;

use std::collections::HashMap;

#[derive(Debug)]
pub struct Environment {
    stack: Vec<HashMap<String, LiteralValue>>,
}

impl Environment {
    pub fn new() -> Environment {
        // initialize the global environment map as the first entry on the stack
        Environment {
            stack: vec![HashMap::new()],
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
            .expect("Missing global scope frame from Environment stack.");
        frame.insert(name, value);
    }

    // TODO: error propagation instead of panic in this function
    fn assign(&mut self, name: String, value: LiteralValue) {
        if let Some(frame) = self.stack.iter_mut().rev().find(|x| x.contains_key(&name)) {
            frame.insert(name, value);
        } else {
            panic!("Undefined variable: {}", name);
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

pub fn execute(statements: &Vec<Stmt>, env: &mut Environment) {
    for statement in statements {
        execute_statement(statement, env)
    }
}

fn execute_statement(statement: &Stmt, env: &mut Environment) {
    match statement {
        Stmt::Block { statements } => block_statement(statements, env),
        Stmt::Expression { expression } => expression_statement(expression, env),
        Stmt::Print { expression } => print_statement(expression, env),
        Stmt::Var { name, initializer } => var_statement(name, initializer, env),
    }
}

fn block_statement(statements: &Vec<Stmt>, env: &mut Environment) {
    env.push();
    for statement in statements {
        execute_statement(statement, env);
    }
    env.pop();
}

fn expression_statement(expression: &Expr, env: &mut Environment) {
    evaluate(expression, env);
}

fn print_statement(expression: &Expr, env: &mut Environment) {
    let result = evaluate(expression, env);
    println!("{}", result.to_string());
}

fn var_statement(name: &Token, initializer: &Expr, env: &mut Environment) {
    let result = evaluate(initializer, env);
    env.define(name.lexeme.clone(), result);
}

// TODO: What is the proper return type here? Do we need custom expression result for later features?
fn evaluate(expression: &Expr, env: &mut Environment) -> LiteralValue {
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

fn evaluate_assign(name: &Token, value: &Box<Expr>, env: &mut Environment) -> LiteralValue {
    let result = evaluate(value, env);
    env.assign(name.lexeme.clone(), result.clone());
    result
}

// TODO: error propagation instead of panic in this function
fn evaluate_binary(
    left: &Box<Expr>,
    operator: &Token,
    right: &Box<Expr>,
    env: &mut Environment,
) -> LiteralValue {
    let left_result = evaluate(left, env);
    let right_result = evaluate(right, env);

    if is_numeric_binary_operation(&operator.token_type) {
        // TODO: error propagation instead of panic
        let left_number = extract_number(&left_result).unwrap();
        let right_number = extract_number(&right_result).unwrap();

        match operator.token_type {
            TokenType::Minus => LiteralValue::Number(left_number - right_number),
            TokenType::Slash => LiteralValue::Number(left_number / right_number),
            TokenType::Star => LiteralValue::Number(left_number * right_number),
            TokenType::Plus => LiteralValue::Number(left_number + right_number),
            TokenType::Greater => LiteralValue::Boolean(left_number > right_number),
            TokenType::GreaterEqual => LiteralValue::Boolean(left_number >= right_number),
            TokenType::Less => LiteralValue::Boolean(left_number < right_number),
            TokenType::LessEqual => LiteralValue::Boolean(left_number <= right_number),
            _ => panic!(
                "Unhandled binary numeric operation type {:?}",
                operator.token_type
            ),
        }
    } else {
        match operator.token_type {
            TokenType::EqualEqual => LiteralValue::Boolean(left_result == right_result),
            TokenType::BangEqual => LiteralValue::Boolean(left_result != right_result),
            _ => panic!(
                "Unhandled binary non-numeric operation type {:?}",
                operator.token_type
            ),
        }
    }
}

fn evaluate_grouping(expression: &Box<Expr>, env: &mut Environment) -> LiteralValue {
    evaluate(expression, env)
}

fn evaluate_literal(value: &LiteralValue) -> LiteralValue {
    value.clone()
}

fn evaluate_unary(operator: &Token, right: &Box<Expr>, env: &mut Environment) -> LiteralValue {
    let operand = evaluate(right, env);
    match operator.token_type {
        TokenType::Bang => LiteralValue::Boolean(!is_truthy(&operand)),
        TokenType::Minus => {
            // TODO: error propagation instead of panic here
            let original = extract_number(&operand).unwrap();
            LiteralValue::Number(-original)
        }
        _ => panic!(
            "Unary expression not implemented in interpreter: {:?}",
            operator
        ),
    }
}

// TODO: error propagation instead of panic in this function
fn evaluate_variable(name: &Token, env: &Environment) -> LiteralValue {
    if let Some(value) = env.get(&name.lexeme) {
        value.clone()
    } else {
        panic!("Variable {} not found", name.lexeme)
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
