use ariadne::{ColorGenerator, Label, Report, ReportKind, Source};
use logos::Span;
use std::io;

pub fn report_error(src: String, filename: &str, msg: String, span: Span) -> io::Result<()> {
    let mut color_generator = ColorGenerator::new();
    let color = color_generator.next();
    Report::build(ReportKind::Error, &filename, 12)
        .with_message("Oh uh!".to_string())
        .with_label(
            Label::new((&filename, span))
                .with_message(msg)
                .with_color(color),
        )
        .finish()
        .eprint((&filename, Source::from(src)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn try_report_error() {
        let src = "let foo: bool = 'c'".to_string();
        let filename = "abc.nln";
        let _ = report_error(src, filename, "Expected 'char' type.".to_string(), 9..13);
    }
}
