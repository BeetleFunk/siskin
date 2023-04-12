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
        TokenType::Var => var_declaration(cursor),
        _ => statement(cursor),
    }
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
    if let Some(token) = cursor.advance_if_any_match(&[TokenType::If, TokenType::Print, TokenType::LeftBrace]) {
        match token.token_type {
            TokenType::If => if_statement(cursor),
            TokenType::Print => print_statement(cursor),
            TokenType::LeftBrace => block_statement(cursor),
            _ => Err(Box::new(build_error("Unexpected token type when parsing statement.", token.line)))
        }
    } else {
        expression_statement(cursor)
    }
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
        .ok_or_else(|| build_error("Expect ';' after expression.", cursor.peek().line))?;
    Ok(Stmt::Print { expression })
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
    // TODO: Good way to avoid making this mutable?
    let mut expr = equality(cursor)?;

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

    primary(cursor)
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
