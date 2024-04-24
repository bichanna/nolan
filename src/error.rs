use core::fmt;
use std::fs::read_to_string;
use std::num::IntErrorKind::*;
use std::num::{ParseFloatError, ParseIntError};
use std::path::{Path, PathBuf};

use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use logos;
use snailquote::UnescapeError;
use thiserror::Error;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourcePath(pub PathBuf);

pub type Span = logos::Span;
pub trait Reportable: fmt::Display {
    fn get_span(&self) -> &Span;
}

impl fmt::Display for SourcePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0.to_string_lossy())
    }
}

impl AsRef<Path> for SourcePath {
    fn as_ref(&self) -> &Path {
        self.0.as_path()
    }
}

pub fn report_errors<E: Reportable>(source: SourcePath, errors: Vec<E>) {
    let mut color_gen = ColorGenerator::new();

    let labels: Vec<Label<(SourcePath, Span)>> = errors
        .iter()
        .map(|err| {
            Label::new((source.clone(), err.get_span().clone()))
                .with_color(color_gen.next())
                .with_message(err.to_string())
        })
        .collect();

    let src = read_to_string(source.clone())
        .unwrap_or_else(|_| panic!("failed to read '{}'", source));

    let result = Report::build(ReportKind::Error, source.clone(), 12)
        .with_labels(labels)
        .finish()
        .eprint((source, Source::from(src)));

    if let Err(err) = result {
        panic!("Couldn't report errors: {}", err);
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Spanned<T>(pub T, pub Span);

pub fn combine(lhs: &Span, rhs: &Span) -> Span {
    logos::Span {
        start: if lhs.start < rhs.start { lhs.start } else { rhs.start },
        end: if lhs.end < rhs.end { rhs.end } else { lhs.end },
    }
}

#[derive(Error, Debug, Clone, PartialEq, Default)]
pub enum LexError {
    #[error("Invalid escape sequence: {0}")]
    InvalidEscapeSequence(String),

    #[error("{0}")]
    InvalidInt(String),

    #[error("Invalid float")]
    InvalidFloat,

    #[error("{0}")]
    WithMessage(String),

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

pub type TypeCheckError = Spanned<String>;

impl Reportable for TypeCheckError {
    fn get_span(&self) -> &Span {
        &self.1
    }
}

impl fmt::Display for TypeCheckError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}
