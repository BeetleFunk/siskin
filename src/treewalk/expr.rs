use std::fmt;

use crate::scanner::{Token, TokenType};

#[derive(Debug, Clone, PartialEq)]
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
    Call {
        callee: Box<Expr>,
        paren: Token,
        arguments: Vec<Expr>,
    },
    Grouping {
        expression: Box<Expr>,
    },
    Literal {
        value: LiteralValue,
    },
    Logical {
        left: Box<Expr>,
        operator: Token,
        right: Box<Expr>,
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
        write!(
            f,
            "{}",
            match self {
                Self::Boolean(value) => value.to_string(),
                Self::Nil => "nil".to_string(),
                Self::Number(value) => value.to_string(),
                Self::String(value) => value.to_string(),
            }
        )
    }
}

impl TryFrom<&Token> for LiteralValue {
    type Error = &'static str;
    fn try_from(token: &Token) -> Result<Self, Self::Error> {
        match token.token_type {
            TokenType::False => Ok(LiteralValue::Boolean(false)),
            TokenType::True => Ok(LiteralValue::Boolean(true)),
            TokenType::Nil => Ok(LiteralValue::Nil),
            TokenType::Number => Ok(LiteralValue::Number(token.extract_number())),
            TokenType::String => Ok(LiteralValue::String(token.extract_string().clone())),
            _ => Err("A trivial conversion into LiteralValue is not implemented for this type of Token."),
        }
    }
}

// pub fn print_ast(root: &Expr) -> String {
//     let mut printed = String::new();
//     format_expr(root, &mut printed);
//     printed
// }

// fn format_expr(expr: &Expr, output: &mut String) {
//     match expr {
//         Expr::Assign { name, value } => {
//             format_subexpr(&name.lexeme, value, output);
//         }
//         Expr::Binary {
//             left,
//             operator,
//             right,
//         } => {
//             format_subexprs(&operator.lexeme, left, right, output);
//         }
//         Expr::Grouping { expression } => {
//             format_subexpr("group", expression, output);
//         }
//         Expr::Literal { value } => {
//             output.push_str(&value.to_string());
//         }
//         Expr::Unary { operator, right } => {
//             format_subexpr(&operator.lexeme, right, output);
//         }
//         Expr::Variable { name } => {
//             output.push_str(&name.lexeme);
//         }
//     }
// }

// fn format_subexpr(name: &str, expr: &Expr, output: &mut String) {
//     output.push('(');
//     output.push_str(name);
//     output.push(' ');
//     format_expr(&expr, output);
//     output.push(')');
// }

// fn format_subexprs(name: &str, expr1: &Expr, expr2: &Expr, output: &mut String) {
//     output.push('(');
//     output.push_str(name);
//     output.push(' ');
//     format_expr(expr1, output);
//     output.push(' ');
//     format_expr(expr2, output);
//     output.push(')');
// }
