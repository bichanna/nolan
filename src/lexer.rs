use logos::{skip, Logos};
use snailquote::unescape;

use crate::error::LexError;

#[derive(Logos, Clone, Debug, PartialEq)]
#[logos(error = LexError)]
pub enum Token {
    /// Identifier
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*", |lex| lex.slice().to_string(), priority = 1)]
    Ident(String),

    /// String
    /// Regex inspired by <https://stackoverflow.com/questions/32155133/regex-to-match-a-json-string>
    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| unescape(lex.slice()))]
    Str(String),

    /// Integer
    #[regex(r"-?(?:0|[1-9]\d*)(?:[eE]?\d+)?", |lex| lex.slice().parse::<i64>())]
    Int(i64),

    /// Float
    #[regex(r"-?(?:0|[1-9]\d*)\.\d+(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>())]
    Float(f64),

    #[regex(r"[ \n\t\f]+", skip)]
    #[regex(r"//[^\n]*\n?", skip)]
    #[regex(r"/\*(?:[^*]|\*[^/])*\*/", skip)] // Can't be nested
    #[token("(")]
    LeftParen,

    #[token(")")]
    RightParen,

    #[token("{")]
    LeftBrace,

    #[token("}")]
    RightBrace,

    #[token("[")]
    LeftBrak,

    #[token("]")]
    RightBrak,

    #[token("\\")]
    BackSlash,

    #[token("'")]
    SingleQuote,

    #[token("|")]
    MatchOr,

    #[token("+")]
    Plus,

    #[token("+=")]
    PlusEq,

    #[token("-")]
    Minus,

    #[token("-=")]
    MinusEq,

    #[token("*")]
    Mul,

    #[token("*=")]
    MulEq,

    #[token("/")]
    Div,

    #[token("/=")]
    DivEq,

    #[token("%")]
    Rem,

    #[token("%=")]
    RemEq,

    #[token(",")]
    Comma,

    #[token(">")]
    GT,

    #[token(">=")]
    GE,

    #[token("<")]
    LT,

    #[token("<=")]
    LE,

    #[token("<.")]
    LDot,

    #[token("!=")]
    NotEq,

    #[token("=")]
    Eq,

    #[token("==")]
    DEq,

    #[token(".")]
    Dot,

    #[token(":")]
    Colon,

    #[token("::")]
    DColon,

    #[token("_")]
    Underscore,

    // Keywords
    #[token("not")]
    Not,

    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("then")]
    Then,

    #[token("do")]
    Do,

    #[token("pure")]
    Pure,

    #[token("rec")]
    Rec,

    #[token("func")]
    Func,

    #[token("match")]
    Match,

    #[token("let")]
    Let,

    #[token("enum")]
    Enum,

    #[token("struct")]
    Struct,

    #[token("while")]
    While,

    #[token("break")]
    Break,

    #[token("continue")]
    Continue,

    #[token("return")]
    Return,

    #[token("use")]
    Use,

    #[token("export")]
    Export,

    #[token("true")]
    True,

    #[token("false")]
    False,

    #[token("and")]
    And,

    #[token["or"]]
    Or,

    // Primitive types
    #[token("str")]
    TStr,

    #[token("int")]
    TInt,

    #[token("float")]
    TFloat,

    #[token("bool")]
    TBool,

    #[token("void")]
    Void,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keywords() {
        let src = "not if else then do pure rec func match let enum struct while break continue return use export true false and or";
        let mut lexer = Token::lexer(&src);

        assert_eq!(lexer.next(), Some(Ok(Token::Not)));
        assert_eq!(lexer.next(), Some(Ok(Token::If)));
        assert_eq!(lexer.next(), Some(Ok(Token::Else)));
        assert_eq!(lexer.next(), Some(Ok(Token::Then)));
        assert_eq!(lexer.next(), Some(Ok(Token::Do)));
        assert_eq!(lexer.next(), Some(Ok(Token::Pure)));
        assert_eq!(lexer.next(), Some(Ok(Token::Rec)));
        assert_eq!(lexer.next(), Some(Ok(Token::Func)));
        assert_eq!(lexer.next(), Some(Ok(Token::Match)));
        assert_eq!(lexer.next(), Some(Ok(Token::Let)));
        assert_eq!(lexer.next(), Some(Ok(Token::Enum)));
        assert_eq!(lexer.next(), Some(Ok(Token::Struct)));
        assert_eq!(lexer.next(), Some(Ok(Token::While)));
        assert_eq!(lexer.next(), Some(Ok(Token::Break)));
        assert_eq!(lexer.next(), Some(Ok(Token::Continue)));
        assert_eq!(lexer.next(), Some(Ok(Token::Return)));
        assert_eq!(lexer.next(), Some(Ok(Token::Use)));
        assert_eq!(lexer.next(), Some(Ok(Token::Export)));
        assert_eq!(lexer.next(), Some(Ok(Token::True)));
        assert_eq!(lexer.next(), Some(Ok(Token::False)));
        assert_eq!(lexer.next(), Some(Ok(Token::And)));
        assert_eq!(lexer.next(), Some(Ok(Token::Or)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn types() {
        let src = "str int float bool void";
        let mut lexer = Token::lexer(&src);

        assert_eq!(lexer.next(), Some(Ok(Token::TStr)));
        assert_eq!(lexer.next(), Some(Ok(Token::TInt)));
        assert_eq!(lexer.next(), Some(Ok(Token::TFloat)));
        assert_eq!(lexer.next(), Some(Ok(Token::TBool)));
        assert_eq!(lexer.next(), Some(Ok(Token::Void)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn others() {
        let src = r#"
            _someId123 "Some String!!\n" 123 1.23e2
            // some comments here!
            /* hello world */ abc
        "#;
        let mut lexer = Token::lexer(&src);

        assert_eq!(
            lexer.next(),
            Some(Ok(Token::Ident("_someId123".to_string())))
        );
        assert_eq!(
            lexer.next(),
            Some(Ok(Token::Str("Some String!!\n".to_string())))
        );
        assert_eq!(lexer.next(), Some(Ok(Token::Int(123))));
        assert_eq!(lexer.next(), Some(Ok(Token::Float(1.23e2))));
        assert_eq!(lexer.next(), Some(Ok(Token::Ident("abc".to_string()))));
        assert_eq!(lexer.next(), None);
    }
}
