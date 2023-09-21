use std::result;

use crate::error::BasicError;
use crate::scanner::{Scanner, Token, TokenType};

pub fn compile(source: &str) -> result::Result<(), BasicError> {
    let mut scanner = Scanner::new(source);

    loop {
        let token = scanner.next_token()?;

        if token.token_type == TokenType::Eof {
            break;
        }
    }

    Ok(())
}
