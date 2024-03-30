use std::num::IntErrorKind::*;
use std::num::{ParseFloatError, ParseIntError};

use snailquote::UnescapeError;
use thiserror::Error;

#[derive(Error, Debug, Clone, PartialEq, Default)]
pub enum LexError {
    #[error("Invalid escape sequence: {0}")]
    InvalidEscapeSequence(String),

    #[error("{0}")]
    InvalidInt(String),

    #[error("Invalid float")]
    InvalidFloat,

    #[error("Lexing error")]
    #[default]
    Other,
}

impl From<ParseIntError> for LexError {
    fn from(value: ParseIntError) -> Self {
        match value.kind() {
            PosOverflow | NegOverflow => {
                LexError::InvalidInt("Overflow error".to_string())
            }
            InvalidDigit => LexError::InvalidInt("Invalid digit".to_string()),
            _ => LexError::InvalidInt("Invalid integer".to_string()),
        }
    }
}

impl From<ParseFloatError> for LexError {
    fn from(_: ParseFloatError) -> Self {
        LexError::InvalidFloat
    }
}

impl From<UnescapeError> for LexError {
    fn from(value: UnescapeError) -> Self {
        LexError::InvalidEscapeSequence(format!("{}", value))
    }
}
