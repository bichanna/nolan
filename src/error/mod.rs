use core::fmt;
use std::fs::read_to_string;
use std::path::{Path, PathBuf};

use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use logos;

pub mod ignored;
pub mod lex;
pub mod parse;

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct SourcePath(PathBuf);

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
        .expect(&format!("failed to read '{}'", source));

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
