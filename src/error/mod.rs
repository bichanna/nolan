use logos;

pub type Span = logos::Span;

#[derive(Clone, Debug)]
pub struct Spanned<T>(pub T, pub Span);

pub fn combine(lhs: Span, rhs: Span) -> Span {
    Span {
        start: if lhs.start < rhs.start {
            lhs.start
        } else {
            rhs.start
        },
        end: if lhs.end < rhs.end { rhs.end } else { lhs.end },
    }
}
