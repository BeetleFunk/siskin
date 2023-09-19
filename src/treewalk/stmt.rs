use crate::scanner::Token;

use super::expr::Expr;

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Block {
        statements: Vec<Stmt>,
    },
    Expression {
        expression: Expr,
    },
    Function {
        name: Token,
        params: Vec<Token>,
        body: Box<Stmt>,
    },
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
    },
    Print {
        expression: Expr,
    },
    Return {
        line: u32,
        expression: Expr,
    },
    Var {
        name: Token,
        initializer: Expr,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
    },
}
