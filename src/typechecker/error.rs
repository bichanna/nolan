use logos::Span;
use thiserror::Error;

use crate::typechecker::Type;

#[derive(Clone, Error, Debug, PartialEq)]
#[error("{err_type}")]
pub struct SemanticError {
    err_type: SemanticErrorType,
    pub span: Span,
}

#[derive(Clone, Error, PartialEq, Debug)]
pub enum SemanticErrorType {
    #[error("expected {0} but got {1}")]
    UnexpectedTypeError(Type, Type),

    #[error("undefined '{0}'")]
    UndefinedError(String),

    #[error("invalid type '{0}'")]
    InvalidTypeError(Type),
}

impl SemanticError {
    pub fn new(err_type: SemanticErrorType, span: Span) -> Self {
        Self { err_type, span }
    }
}
