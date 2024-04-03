use core::fmt;

use crate::error::lex::LexError;
use crate::error::{Reportable, Span, Spanned};

pub type ParseError = Spanned<ParseErrorKind>;

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind {
    LexErr(LexError),
    ParseErr(String),
}

impl Reportable for ParseError {
    fn get_span(&self) -> &Span {
        &self.1
    }
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let res = match &self.0 {
            ParseErrorKind::LexErr(err) => format!("{err}"),
            ParseErrorKind::ParseErr(err) => err.to_string(),
        };

        write!(f, "{res}")
    }
}
