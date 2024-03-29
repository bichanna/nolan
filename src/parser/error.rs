use logos::Span;
use thiserror::Error;

use crate::lexer::error::LexError;
use crate::lexer::Token;

/// The error thrown when a parsing error occurs.
#[derive(Clone, Error, Debug, PartialEq)]
#[error("{err_type}")]
pub struct ParseError {
    err_type: ParseErrorType,
    pub span: Span,
}

#[derive(Clone, Error, PartialEq, Debug)]
pub enum ParseErrorType {
    #[error("{0}")]
    LexError(LexError),

    #[error("Expected {0}")]
    ExpectedDiffTokenError(String),

    #[error("Invalid type: {0}")]
    InvalidTypeError(String),

    #[error("Exceeded max argument length")]
    MaxArgLenError,

    #[error("Invalid token: {0:?}")]
    InvalidTokenError(Token),
}

impl ParseError {
    pub fn new(err_type: ParseErrorType, span: Span) -> Self {
        ParseError { err_type, span }
    }
}

impl From<LexError> for ParseErrorType {
    fn from(value: LexError) -> Self {
        ParseErrorType::LexError(value)
    }
}
