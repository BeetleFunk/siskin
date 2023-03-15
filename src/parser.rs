use crate::expr::Expr;
use crate::scanner::Token;
use crate::scanner::TokenType;
use crate::stmt::Stmt;

pub fn parse(tokens: &Vec<Token>) -> Vec<Stmt> {
    let mut cursor = TokenCursor::new(tokens);
    let mut statements = Vec::new();

    while !cursor.at_end() {
        statements.push(declaration(&mut cursor));
    }

    statements
}

fn declaration(cursor: &mut TokenCursor) -> Stmt {
    match cursor.peek().token_type {
        TokenType::Var => { var_declaration(cursor) }
        _ => statement(cursor)
    }
}

fn var_declaration(cursor: &mut TokenCursor) -> Stmt {
    cursor.advance_if_match(&TokenType::Var).expect("Parsing var declaration with invalid token");

    // TODO: this can be cleaner after literal restructuring
    let current = cursor.peek();
    let name = match current.token_type {
        TokenType::Identifier(_) => current.clone(),
        _ => panic!("Expect variable name.")
    };

    let initializer = if cursor.advance_if_match(&TokenType::Equal).is_some() {
        expression(cursor)
    } else {
        Expr::Literal { value: Token { token_type: TokenType::Nil, lexeme: "".to_string(), line: 0} }
    };

    cursor.advance_if_match(&TokenType::Semicolon).expect("Expect ';' after variable declaration.");

    Stmt::Var { name, initializer }
}

fn statement(cursor: &mut TokenCursor) -> Stmt {
    match cursor.peek().token_type {
        TokenType::Print => { print_statement(cursor) }
        TokenType::LeftBrace => { block_statement(cursor) }
        _ => expression_statement(cursor)
    }
}

fn print_statement(cursor: &mut TokenCursor) -> Stmt {
    cursor.advance_if_match(&TokenType::Print).expect("Parsing print statement with invalid token");
    let expression = expression(cursor);
    cursor.advance_if_match(&TokenType::Semicolon).expect("Expect ';' after expression.");
    Stmt::Print { expression }
}

fn expression_statement(cursor: &mut TokenCursor) -> Stmt {
    let expression = expression(cursor);
    cursor.advance_if_match(&TokenType::Semicolon).expect("Expect ';' after expression.");
    Stmt::Expression { expression }
}

fn block_statement(cursor: &mut TokenCursor) -> Stmt {
    cursor.advance_if_match(&TokenType::LeftBrace).expect("Parsing block statement with invalid token");

    let mut statements = Vec::new();
    while cursor.advance_if_match(&TokenType::RightBrace).is_none() {
        statements.push(declaration(cursor))
    }

    Stmt::Block { statements }
}

fn expression(cursor: &mut TokenCursor) -> Expr {
    assignment(cursor)
}

fn assignment(cursor: &mut TokenCursor) -> Expr {
    // TODO: Good way to avoid making this mutable?
    let mut expr = equality(cursor);

    let equal = cursor.advance_if_match(&TokenType::Equal);
    if equal.is_some() {
        let value = assignment(cursor);

        match expr {
            Expr::Variable { name } => expr = Expr::Assign { name, value: Box::new(value) },
            _ => panic!("Invalid assignment target at line {}", equal.unwrap().line)
        };
    }

    expr
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

    while let Some(operator) = cursor.advance_if_any_match(types) {
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
    while let Some(operator) = cursor.advance_if_any_match(&[TokenType::Bang, TokenType::Minus]) {
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
            let right_paren = cursor.advance_if_any_match(&[TokenType::RightParen]);
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
