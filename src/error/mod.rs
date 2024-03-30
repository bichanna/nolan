use std::path::Path;
use std::rc::Rc;

use logos;

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    pub span: logos::Span,
    pub path: Rc<Path>,
}

impl Span {
    pub fn proper_span(span: logos::Span, path: Rc<Path>) -> Self {
        Self { span, path }
    }
}

#[derive(Clone, Debug)]
pub struct Spanned<T>(pub T, pub Span);

pub fn combine(lhs: Span, rhs: Span) -> Span {
    let (lhs, rhs, path) = (lhs.span, rhs.span, lhs.path);
    Span {
        span: logos::Span {
            start: if lhs.start < rhs.start { lhs.start } else { rhs.start },
            end: if lhs.end < rhs.end { rhs.end } else { lhs.end },
        },
        path,
    }
}
