use std::collections::HashSet;
use std::result;

use crate::error::BasicError;
use crate::scanner::Token;

use super::expr::Expr;
use super::expr::LiteralValue;
use super::stmt::Stmt;

type UnitResult = result::Result<(), BasicError>;

struct VarScopes {
    stack: Vec<HashSet<String>>,
    unresolved_vars: Vec<Token>,
}

impl VarScopes {
    fn new(initial_definitions: HashSet<String>) -> VarScopes {
        VarScopes {
            stack: vec![initial_definitions],
            unresolved_vars: Vec::new(),
        }
    }

    fn push(&mut self) {
        self.stack.push(HashSet::new());
    }

    fn pop(&mut self) {
        self.stack
            .pop()
            .expect("Attempted to pop empty VarScopes stack."); // panic here because this would indicate a bug in the interpreter
    }

    fn define(&mut self, name: String) {
        let frame = self
            .stack
            .last_mut()
            .expect("Missing toplevel scope frame from VarScopes stack."); // panic here because this would indicate a bug in the interpreter
        frame.insert(name);
    }

    fn var_is_defined(&self, name: &str) -> bool {
        for frame in self.stack.iter().rev() {
            if frame.contains(name) {
                return true;
            }
        }
        false
    }
}

pub fn resolve_function_captures(
    params: &[Token],
    body: &Stmt,
) -> result::Result<Vec<Token>, BasicError> {
    let initial_definitions = HashSet::from_iter(params.iter().map(|token| token.lexeme.clone()));
    let mut scope_info = VarScopes::new(initial_definitions);
    resolve_statement(body, &mut scope_info)?;
    Ok(scope_info.unresolved_vars)
}

fn resolve_statement(statement: &Stmt, scope: &mut VarScopes) -> UnitResult {
    match statement {
        Stmt::Block { statements } => block_statement(statements, scope),
        Stmt::Expression { expression } => expression_statement(expression, scope),
        Stmt::Function { name, params, body } => function_statement(name, params, body, scope),
        Stmt::If {
            condition,
            then_branch,
            else_branch,
        } => if_statement(condition, then_branch, else_branch, scope),
        Stmt::Print { expression } => print_statement(expression, scope),
        Stmt::Return {
            line: _,
            expression,
        } => return_statement(expression, scope),
        Stmt::Var { name, initializer } => var_statement(name, initializer, scope),
        Stmt::While { condition, body } => while_statement(condition, body, scope),
    }
}

fn block_statement(statements: &[Stmt], scope: &mut VarScopes) -> UnitResult {
    scope.push();
    for statement in statements {
        // assuming we bail completely and don't need to pop the stack if an error occurs in here
        resolve_statement(statement, scope)?;
    }
    scope.pop();
    Ok(())
}

fn expression_statement(expression: &Expr, scope: &mut VarScopes) -> UnitResult {
    resolve_expr(expression, scope)?;
    Ok(())
}

fn function_statement(
    name: &Token,
    _params: &[Token],
    body: &Stmt,
    scope: &mut VarScopes,
) -> UnitResult {
    scope.define(name.lexeme.clone());
    resolve_statement(body, scope)
}

fn if_statement(
    condition: &Expr,
    then_branch: &Stmt,
    else_branch: &Option<Box<Stmt>>,
    scope: &mut VarScopes,
) -> UnitResult {
    resolve_expr(condition, scope)?;
    resolve_statement(then_branch, scope)?;
    if let Some(else_statement) = else_branch {
        resolve_statement(else_statement, scope)?
    }
    Ok(())
}

fn print_statement(expression: &Expr, scope: &mut VarScopes) -> UnitResult {
    resolve_expr(expression, scope)
}

fn return_statement(expression: &Expr, scope: &mut VarScopes) -> UnitResult {
    resolve_expr(expression, scope)
}

fn var_statement(name: &Token, initializer: &Expr, scope: &mut VarScopes) -> UnitResult {
    resolve_expr(initializer, scope)?;
    scope.define(name.lexeme.clone());
    Ok(())
}

fn while_statement(condition: &Expr, body: &Stmt, scope: &mut VarScopes) -> UnitResult {
    resolve_expr(condition, scope)?;
    resolve_statement(body, scope)
}

fn resolve_expr(expression: &Expr, scope: &mut VarScopes) -> UnitResult {
    match expression {
        Expr::Assign { name, value } => resolve_assign(name, value, scope),
        Expr::Binary {
            left,
            operator,
            right,
        } => resolve_binary(left, operator, right, scope),
        Expr::Call {
            callee,
            paren,
            arguments,
        } => resolve_call(callee, paren, arguments, scope),
        Expr::Grouping { expression } => resolve_grouping(expression, scope),
        Expr::Literal { value } => resolve_literal(value),
        Expr::Logical {
            left,
            operator,
            right,
        } => resolve_logical(left, operator, right, scope),
        Expr::Unary { operator, right } => resolve_unary(operator, right, scope),
        Expr::Variable { name } => resolve_variable(name, scope),
    }
}

fn resolve_assign(_name: &Token, value: &Expr, scope: &mut VarScopes) -> UnitResult {
    resolve_expr(value, scope)
}

fn resolve_binary(
    left: &Expr,
    _operator: &Token,
    right: &Expr,
    scope: &mut VarScopes,
) -> UnitResult {
    resolve_expr(left, scope)?;
    resolve_expr(right, scope)
}

fn resolve_call(
    callee: &Expr,
    _paren: &Token,
    arguments: &[Expr],
    scope: &mut VarScopes,
) -> UnitResult {
    resolve_expr(callee, scope)?;
    for argument in arguments {
        resolve_expr(argument, scope)?;
    }
    Ok(())
}

fn resolve_grouping(expression: &Expr, scope: &mut VarScopes) -> UnitResult {
    resolve_expr(expression, scope)
}

fn resolve_literal(_value: &LiteralValue) -> UnitResult {
    Ok(())
}

fn resolve_logical(
    left: &Expr,
    _operator: &Token,
    right: &Expr,
    scope: &mut VarScopes,
) -> UnitResult {
    resolve_expr(left, scope)?;
    resolve_expr(right, scope)
}

fn resolve_unary(_operator: &Token, right: &Expr, scope: &mut VarScopes) -> UnitResult {
    resolve_expr(right, scope)
}

fn resolve_variable(name: &Token, scope: &mut VarScopes) -> UnitResult {
    if !scope.var_is_defined(&name.lexeme) {
        scope.unresolved_vars.push(name.clone());
    }
    Ok(())
}
