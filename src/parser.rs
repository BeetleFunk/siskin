use crate::error::BasicError;
use crate::error::GenericResult;
use crate::expr::Expr;
use crate::expr::LiteralValue;
use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::stmt::Stmt;

type StmtResult = GenericResult<Stmt>;
type ExprResult = GenericResult<Expr>;

pub fn parse(tokens: &Vec<Token>) -> GenericResult<Vec<Stmt>> {
    let mut cursor = TokenCursor::new(tokens);
    let mut statements = Vec::new();

    while !cursor.at_end() {
        statements.push(declaration(&mut cursor)?);
    }

    Ok(statements)
}

fn declaration(cursor: &mut TokenCursor) -> StmtResult {
    match cursor.peek().token_type {
        TokenType::Fun => function(cursor),
        TokenType::Var => var_declaration(cursor),
        _ => statement(cursor),
    }
}

fn function(cursor: &mut TokenCursor) -> StmtResult {
    cursor
        .advance_if_match(&TokenType::Fun)
        .ok_or_else(|| build_error("Function declaration with invalid token.", cursor.peek().line))?;

    let name = if matches!(cursor.peek().token_type, TokenType::Identifier(_)) {
        let clone = cursor.peek().clone();
        cursor.advance();
        clone
    } else {
        return Err(Box::new(build_error("Expect function name.", cursor.peek().line)));
    };

    // parse the parameter list
    let mut params = Vec::new();
    cursor
        .advance_if_match(&TokenType::LeftParen)
        .ok_or_else(|| build_error("Expect '(' after function name.", cursor.peek().line))?;
    if cursor.peek().token_type != TokenType::RightParen {
        loop {
            if matches!(cursor.peek().token_type, TokenType::Identifier(_)) {
                params.push(cursor.peek().clone());
                cursor.advance();
            } else {
                return Err(Box::new(build_error("Expected identifiers only in parameter list.", cursor.peek().line)));
            }
            
            // keep grabbing the next argument as long as the following token is a comma
            if cursor.advance_if_match(&TokenType::Comma).is_none() {
                break;
            }
        }
    }
    cursor
        .advance_if_match(&TokenType::RightParen)
        .ok_or_else(|| build_error("Expect ')' after parameters.", cursor.peek().line))?;

    // parse the function body
    cursor
        .advance_if_match(&TokenType::LeftBrace)
        .ok_or_else(|| build_error("Expect '{' before function body.", cursor.peek().line))?;
    let body = Box::new(block_statement(cursor)?);

    Ok(Stmt::Function { name, params, body })
}

fn var_declaration(cursor: &mut TokenCursor) -> StmtResult {
    cursor
        .advance_if_match(&TokenType::Var)
        .ok_or_else(|| build_error("Variable declaration with invalid token.", cursor.peek().line))?;

    // TODO: this can be cleaner after literal restructuring
    let current = cursor.peek();
    let name = match current.token_type {
        TokenType::Identifier(_) => {
            let clone = current.clone();
            cursor.advance();
            clone
        }
        _ => return Err(Box::new(build_error("Expect variable name.", current.line))),
    };

    let initializer = if cursor.advance_if_match(&TokenType::Equal).is_some() {
        expression(cursor)?
    } else {
        // uninitialized variables default to nil
        Expr::Literal {
            value: LiteralValue::Nil,
        }
    };

    cursor
        .advance_if_match(&TokenType::Semicolon)
        .ok_or_else(|| build_error("Expect ';' after variable declaration.", name.line))?;

    Ok(Stmt::Var { name, initializer })
}

fn statement(cursor: &mut TokenCursor) -> StmtResult {
    if let Some(token) = cursor.advance_if_any_match(&[TokenType::For, TokenType::If, TokenType::Print, TokenType::While, TokenType::LeftBrace]) {
        match token.token_type {
            TokenType::For => for_statement(cursor),
            TokenType::If => if_statement(cursor),
            TokenType::Print => print_statement(cursor),
            TokenType::While => while_statement(cursor),
            TokenType::LeftBrace => block_statement(cursor),
            _ => Err(Box::new(build_error("Unexpected token type when parsing statement.", token.line)))
        }
    } else {
        expression_statement(cursor)
    }
}

fn for_statement(cursor: &mut TokenCursor) -> StmtResult {
    cursor
        .advance_if_match(&TokenType::LeftParen)
        .ok_or_else(|| build_error("Expect '(' after 'for''.", cursor.peek().line))?;

    let initializer = match cursor.peek().token_type {
        TokenType::Semicolon => { cursor.advance(); Stmt::Block { statements: Vec::new() } }, // stand-in for empty statement
        TokenType::Var => var_declaration(cursor)?,
        _ => expression_statement(cursor)?,
    };

    let condition = if cursor.peek().token_type == TokenType::Semicolon {
        Expr::Literal { value: LiteralValue::Boolean(true) } // empty condition defaults to true (infinite looping)
    } else {
        expression(cursor)?
    };
    cursor
        .advance_if_match(&TokenType::Semicolon)
        .ok_or_else(|| build_error("Expect ';' after loop condition.", cursor.peek().line))?;

    let increment = if cursor.peek().token_type == TokenType::RightParen {
        Expr::Literal { value: LiteralValue::Nil } // stand-in for empty expr
    } else {
        expression(cursor)?
    };
    cursor
        .advance_if_match(&TokenType::RightParen)
        .ok_or_else(|| build_error("Expect ')' after for clauses.", cursor.peek().line))?;

    let for_body = statement(cursor)?;

    // transform for loop components into equivalent block statement containing initializer and while loop

    let while_body = Stmt::Block { statements: vec![for_body, Stmt::Expression { expression: increment }] };
    let while_loop = Stmt::While { condition, body: Box::new(while_body) };

    Ok(Stmt::Block { statements: vec![initializer, while_loop] })
}

fn if_statement(cursor: &mut TokenCursor) -> StmtResult {
    cursor
        .advance_if_match(&TokenType::LeftParen)
        .ok_or_else(|| build_error("Expect '(' after 'if''.", cursor.peek().line))?;
    let condition = expression(cursor)?;
    cursor
        .advance_if_match(&TokenType::RightParen)
        .ok_or_else(|| build_error("Expect ')' after if condition.", cursor.peek().line))?;

    let then_branch = Box::new(statement(cursor)?);
    let else_branch = if cursor.advance_if_match(&TokenType::Else).is_some() {
        Some(Box::new(statement(cursor)?))
    } else {
        None
    };

    Ok(Stmt::If { condition, then_branch, else_branch })
}

fn print_statement(cursor: &mut TokenCursor) -> StmtResult {
    let expression = expression(cursor)?;
    cursor
        .advance_if_match(&TokenType::Semicolon)
        .ok_or_else(|| build_error("Expect ';' after print statement.", cursor.peek().line))?;
    Ok(Stmt::Print { expression })
}

fn while_statement(cursor: &mut TokenCursor) -> StmtResult {
    cursor
        .advance_if_match(&TokenType::LeftParen)
        .ok_or_else(|| build_error("Expect '(' after 'while''.", cursor.peek().line))?;
    let condition = expression(cursor)?;
    cursor
        .advance_if_match(&TokenType::RightParen)
        .ok_or_else(|| build_error("Expect ')' after while condition.", cursor.peek().line))?;

    let body = Box::new(statement(cursor)?);
    Ok(Stmt::While { condition, body })
}

fn expression_statement(cursor: &mut TokenCursor) -> StmtResult {
    let expression = expression(cursor)?;
    cursor
        .advance_if_match(&TokenType::Semicolon)
        .ok_or_else(|| build_error("Expect ';' after expression.", cursor.peek().line))?;
    Ok(Stmt::Expression { expression })
}

fn block_statement(cursor: &mut TokenCursor) -> StmtResult {
    let mut statements = Vec::new();
    while cursor.advance_if_match(&TokenType::RightBrace).is_none() {
        statements.push(declaration(cursor)?)
    }
    Ok(Stmt::Block { statements })
}

fn expression(cursor: &mut TokenCursor) -> ExprResult {
    assignment(cursor)
}

fn assignment(cursor: &mut TokenCursor) -> ExprResult {
    let mut expr = or(cursor)?;

    let equal = cursor.advance_if_match(&TokenType::Equal);
    if let Some(equal) = equal {
        let value = assignment(cursor)?;

        match expr {
            Expr::Variable { name } => {
                expr = Expr::Assign {
                    name,
                    value: Box::new(value),
                }
            }
            _ => return Err(Box::new(build_error("Invalid assignment target.", equal.line))),
        };
    }

    Ok(expr)
}

fn or(cursor: &mut TokenCursor) -> ExprResult {
    logical_expression(cursor, and, &TokenType::Or)
}

fn and(cursor: &mut TokenCursor) -> ExprResult {
    logical_expression(cursor, equality, &TokenType::And)
}

fn logical_expression(
    cursor: &mut TokenCursor,
    higher_precedence: fn(&mut TokenCursor) -> ExprResult,
    token_type: &TokenType
) -> ExprResult {
    let mut expr = higher_precedence(cursor)?;

    while let Some(operator) = cursor.advance_if_match(token_type) {
        let right = higher_precedence(cursor)?;
        expr = Expr::Logical {
            left: Box::new(expr),
            operator,
            right: Box::new(right),
        };
    }

    Ok(expr)
}

fn equality(cursor: &mut TokenCursor) -> ExprResult {
    binary_left_associative(
        cursor,
        comparison,
        &[TokenType::BangEqual, TokenType::EqualEqual],
    )
}

fn comparison(cursor: &mut TokenCursor) -> ExprResult {
    binary_left_associative(
        cursor,
        addition,
        &[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ],
    )
}

fn addition(cursor: &mut TokenCursor) -> ExprResult {
    binary_left_associative(cursor, multiplication, &[TokenType::Minus, TokenType::Plus])
}

fn multiplication(cursor: &mut TokenCursor) -> ExprResult {
    binary_left_associative(cursor, unary, &[TokenType::Slash, TokenType::Star])
}

// Parse a binary left associative expression as long as the current token matches one of the given types
fn binary_left_associative(
    cursor: &mut TokenCursor,
    higher_precedence: fn(&mut TokenCursor) -> ExprResult,
    types: &[TokenType],
) -> ExprResult {
    let mut expr = higher_precedence(cursor)?;

    while let Some(operator) = cursor.advance_if_any_match(types) {
        let right = higher_precedence(cursor)?;
        expr = Expr::Binary {
            left: Box::new(expr),
            operator,
            right: Box::new(right),
        };
    }

    Ok(expr)
}

fn unary(cursor: &mut TokenCursor) -> ExprResult {
    while let Some(operator) = cursor.advance_if_any_match(&[TokenType::Bang, TokenType::Minus]) {
        let right = unary(cursor)?;
        return Ok(Expr::Unary {
            operator,
            right: Box::new(right),
        });
    }

    call(cursor)
}

fn call(cursor: &mut TokenCursor) -> ExprResult {
    let mut expr = primary(cursor)?;

    if cursor.advance_if_match(&TokenType::LeftParen).is_some() {
        expr = finish_call(cursor, expr)?;
    }

    Ok(expr)
}

fn finish_call(cursor: &mut TokenCursor, callee: Expr) -> ExprResult {
    let mut arguments = Vec::new();

    if cursor.peek().token_type != TokenType::RightParen {
        loop {
            arguments.push(expression(cursor)?);
            // keep grabbing the next argument as long as the following token is a comma
            if cursor.advance_if_match(&TokenType::Comma).is_none() {
                break;
            }
        }
    }

    let paren = cursor
        .advance_if_match(&TokenType::RightParen)
        .ok_or_else(|| build_error("Expect ')' after expression.", cursor.peek().line))?;

    Ok(Expr::Call { callee: Box::new(callee), paren, arguments })
}

fn primary(cursor: &mut TokenCursor) -> ExprResult {
    let current = cursor.peek();

    match current.token_type {
        TokenType::False
        | TokenType::True
        | TokenType::Nil
        | TokenType::Number(_)
        | TokenType::String(_) => {
            let literal = Expr::Literal {
                value: LiteralValue::from(current),
            };
            cursor.advance();
            Ok(literal)
        }
        TokenType::Identifier(_) => {
            let name = current.clone();
            cursor.advance();
            Ok(Expr::Variable { name })
        }
        TokenType::LeftParen => {
            cursor.advance();
            let expr = expression(cursor)?;
            cursor
                .advance_if_match(&TokenType::RightParen)
                .ok_or_else(|| build_error("Expect ')' after grouping expression.", cursor.peek().line))?;
            Ok(Expr::Grouping {
                expression: Box::new(expr),
            })
        }
        _ => return Err(Box::new(build_error("Expect expression.", current.line))),
    }
}

fn build_error(message: &str, line: u32) -> BasicError {
    BasicError::new(&format!("Parse error at line {line}: {message}"))
}

struct TokenCursor<'a> {
    tokens: &'a Vec<Token>,
    index: usize,
}

impl<'a> TokenCursor<'a> {
    fn new(tokens: &Vec<Token>) -> TokenCursor {
        TokenCursor { tokens, index: 0 }
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.index]
    }

    fn advance(&mut self) {
        self.index += 1;
    }

    fn at_end(&self) -> bool {
        matches!(self.tokens[self.index].token_type, TokenType::EOF)
    }

    fn advance_if_match(&mut self, token_type: &TokenType) -> Option<Token> {
        let token = self.peek();
        if token.token_type == *token_type {
            let cloned = token.clone();
            self.advance();
            Some(cloned)
        } else {
            None
        }
    }

    // TODO: TokenType also wraps literal values which could affect equality comparisons in here, don't use this for literal type tokens right now
    fn advance_if_any_match(&mut self, types: &[TokenType]) -> Option<Token> {
        let token = self.peek();
        if types.contains(&token.token_type) {
            let cloned = token.clone();
            self.advance();
            Some(cloned)
        } else {
            None
        }
    }
}
