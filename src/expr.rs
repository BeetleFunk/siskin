use crate::scanner::Token;
use crate::scanner::TokenType;

use std::fmt;

#[derive(Debug)]
pub enum Expr {
    Assign {
        name: Token,
        value: Box<Expr>,
    },
    Binary {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: LiteralValue,
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Variable {
        name: Token,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub enum LiteralValue {
    Boolean(bool),
    Nil,
    Number(f64),
    String(String),
}

impl fmt::Display for LiteralValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", match self {
            Self::Boolean(value) => value.to_string(),
            Self::Nil => "nil".to_string(),
            Self::Number(value) => value.to_string(),
            Self::String(value) => value.to_string(),
        })
    }
}

// TODO: switch to TryFrom with proper error handling
impl From<&Token> for LiteralValue {
    fn from(token: &Token) -> LiteralValue {
        match &token.token_type {
            TokenType::False => LiteralValue::Boolean(false),
            TokenType::True => LiteralValue::Boolean(true),
            TokenType::Nil => LiteralValue::Nil,
            TokenType::Number(value) => LiteralValue::Number(*value),
            TokenType::String(value) => LiteralValue::String(value.clone()),
            _ => panic!("Cannot make literal value from non-literal token")
        }
    }
}

pub fn print_ast(root: &Expr) -> String {
    let mut printed = String::new();
    format_expr(root, &mut printed);
    printed
}

fn format_expr(expr: &Expr, output: &mut String) {
    match expr {
        Expr::Assign { name, value } => {
            format_subexpr(&name.lexeme, value, output);
        }
        Expr::Binary {
            left,
            operator,
            right,
        } => {
            format_subexprs(&operator.lexeme, left, right, output);
        }
        Expr::Grouping { expression } => {
            format_subexpr("group", expression, output);
        }
        Expr::Literal { value } => {
            output.push_str(&value.to_string());
        }
        Expr::Unary { operator, right } => {
            format_subexpr(&operator.lexeme, right, output);
        }
        Expr::Variable { name } => {
            output.push_str(&name.lexeme);
        }
    }
}

fn format_subexpr(name: &str, expr: &Expr, output: &mut String) {
    output.push('(');
    output.push_str(name);
    output.push(' ');
    format_expr(&expr, output);
    output.push(')');
}

fn format_subexprs(name: &str, expr1: &Expr, expr2: &Expr, output: &mut String) {
    output.push('(');
    output.push_str(name);
    output.push(' ');
    format_expr(expr1, output);
    output.push(' ');
    format_expr(expr2, output);
    output.push(')');
}
