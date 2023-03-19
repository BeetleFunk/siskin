use crate::expr::Expr;
use crate::expr::LiteralValue;
use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::stmt::Stmt;

#[derive(Debug)]
pub struct Environment {
    pub stuff: String,
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

fn block_statement(statements: &Vec<Stmt>, env: &mut Environment) {}

fn expression_statement(expression: &Expr, env: &mut Environment) {
    evaluate(expression, env);
}

fn print_statement(expression: &Expr, env: &mut Environment) {
    let result = evaluate(expression, env);
    println!("{}", result.to_string());
}

fn var_statement(name: &Token, initializer: &Expr, env: &mut Environment) {}

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
        Expr::Literal { value } => evaluate_literal(value, env),
        Expr::Unary { operator, right } => evaluate_unary(operator, right, env),
        Expr::Variable { name } => evaluate_variable(name, env),
    }
}

fn evaluate_assign(name: &Token, value: &Box<Expr>, env: &mut Environment) -> LiteralValue {
    // TODO: save value to variable
    evaluate(value, env)
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
            _ => panic!("Unhandled binary numeric operation type {:?}", operator.token_type),
        }
    } else {
        match operator.token_type {
            TokenType::EqualEqual => LiteralValue::Boolean(left_result == right_result),
            TokenType::BangEqual => LiteralValue::Boolean(left_result != right_result),
            _ => panic!("Unhandled binary non-numeric operation type {:?}", operator.token_type),
        }
    }
}

fn evaluate_grouping(expression: &Box<Expr>, env: &mut Environment) -> LiteralValue {
    evaluate(expression, env)
}

fn evaluate_literal(value: &LiteralValue, env: &mut Environment) -> LiteralValue {
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

fn evaluate_variable(name: &Token, env: &mut Environment) -> LiteralValue {
    // TODO: return value of the variable
    LiteralValue::Nil
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
