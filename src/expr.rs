use crate::scanner::Token;

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
        value: Token, // TODO: use token or create literal enum specifically for Expr?
    },
    Unary {
        operator: Token,
        right: Box<Expr>,
    },
    Variable {
        name: Token,
    },
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
            output.push_str(&value.lexeme);
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
