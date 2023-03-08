use crate::expr::Expr;
use crate::scanner::Token;
use crate::scanner::TokenType;

pub fn parse(tokens: &Vec<Token>) -> Expr {
    let mut cursor = TokenCursor::new(tokens);
    expression(&mut cursor)
}

fn expression(cursor: &mut TokenCursor) -> Expr {
    assignment(cursor)
}

fn assignment(cursor: &mut TokenCursor) -> Expr {
    equality(cursor)

    // TODO implement assignment
}

fn equality(cursor: &mut TokenCursor) -> Expr {
    binary_left_associative(
        cursor,
        comparison,
        &[TokenType::BangEqual, TokenType::EqualEqual],
    )
}

fn comparison(cursor: &mut TokenCursor) -> Expr {
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

fn addition(cursor: &mut TokenCursor) -> Expr {
    binary_left_associative(cursor, multiplication, &[TokenType::Minus, TokenType::Plus])
}

fn multiplication(cursor: &mut TokenCursor) -> Expr {
    binary_left_associative(cursor, unary, &[TokenType::Slash, TokenType::Star])
}

// Parse a binary left associative expression as long as the current token matches one of the given types
fn binary_left_associative(
    cursor: &mut TokenCursor,
    higher_precedence: fn(&mut TokenCursor) -> Expr,
    types: &[TokenType],
) -> Expr {
    let mut expr = higher_precedence(cursor);

    while let Some(operator) = cursor.advance_if_match(types) {
        let right = higher_precedence(cursor);
        expr = Expr::Binary {
            left: Box::new(expr),
            operator,
            right: Box::new(right),
        };
    }

    expr
}

fn unary(cursor: &mut TokenCursor) -> Expr {
    while let Some(operator) = cursor.advance_if_match(&[TokenType::Bang, TokenType::Minus]) {
        let right = unary(cursor);
        return Expr::Unary {
            operator,
            right: Box::new(right),
        };
    }

    primary(cursor)
}

fn primary(cursor: &mut TokenCursor) -> Expr {
    let current = cursor.peek();

    match current.token_type {
        TokenType::False
        | TokenType::True
        | TokenType::Nil
        | TokenType::Number(_)
        | TokenType::String(_)
        | TokenType::Identifier(_) => {
            let value = current.clone();
            cursor.advance();
            Expr::Literal { value }
        }
        TokenType::LeftParen => {
            cursor.advance();
            let expr = expression(cursor);
            let right_paren = cursor.advance_if_match(&[TokenType::RightParen]);
            right_paren.expect("Expect ')' after expression.");
            Expr::Grouping {
                expression: Box::new(expr),
            }
        }
        _ => panic!("Expect expression."),
    }
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

    // TODO: TokenType also wraps literal values which could affect equality comparisons in here, don't use this for literal type tokens right now
    fn advance_if_match(&mut self, types: &[TokenType]) -> Option<Token> {
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
