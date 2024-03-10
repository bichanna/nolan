pub mod error;

use logos::{skip, Lexer, Logos, Skip};
use snailquote::{unescape, UnescapeError};

use crate::lexer::error::LexError;

fn newline_callback(lex: &mut Lexer<Token>) -> Skip {
    lex.extras += 1;
    Skip
}

fn char_callback(lex: &mut Lexer<Token>) -> Result<char, UnescapeError> {
    let unescaped = unescape(lex.slice());
    if let Err(err) = unescaped {
        Err(err)
    } else {
        Ok(unescaped.unwrap().chars().nth(0).unwrap())
    }
}

#[derive(Logos, Debug, PartialEq)]
#[logos(error = LexError)]
#[logos(extras = usize)]
enum Token {
    #[regex(r"[ \t\f]+", skip)]
    #[regex(r"//[^\n]*\n?", skip)]
    #[regex(r"/\*(?:[^*]|\*[^/])*\*/", skip)] // Can't be nested
    #[token(";")]
    #[token("\n", newline_callback)]
    NewLine,

    /// Identifier
    #[regex(r"[A-Za-z_][A-Za-z0-9_]+", |lex| lex.slice().to_string(), priority = 1)]
    Ident(String),

    /// String
    /// Regex inspired by <https://stackoverflow.com/questions/32155133/regex-to-match-a-json-string>
    #[regex(r#""([^"\\]|\\["\\bnfrt]|u[a-fA-F0-9]{4})*""#, |lex| unescape(lex.slice()))]
    Str(String),

    /// Character
    #[regex(r#"'([^'\\]|\\['\\bnfrt]|u[a-fA-F0-9]{4})'"#, char_callback)]
    Char(char),

    /// Integer
    #[regex(r"-?(?:0|[1-9]\d*)(?:[eE]?\d+)?", |lex| lex.slice().parse::<i64>())]
    Int(i64),

    /// Float
    #[regex(r"-?(?:0|[1-9]\d*)\.\d+(?:[eE][+-]?\d+)?", |lex| lex.slice().parse::<f64>())]
    Float(f64),

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

    #[token("!")]
    Neg,

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

    // Keywords
    #[token("if")]
    If,

    #[token("else")]
    Else,

    #[token("then")]
    Then,

    #[token("func")]
    Func,

    #[token("match")]
    Match,

    #[token("let")]
    Let,

    #[token("record")]
    Record,

    #[token("enum")]
    Enum,

    #[token("type")]
    Type,

    #[token("while")]
    While,

    #[token("break")]
    Break,

    #[token("continue")]
    Continue,

    #[token("import")]
    Import,

    #[token("export")]
    Export,

    #[token("true")]
    True,

    #[token("false")]
    False,

    // Primitive types
    #[token("str")]
    TStr,

    #[token("char")]
    TChar,

    #[token("byte")]
    TByte,

    #[token("int")]
    TInt,

    #[token("float")]
    TFloat,

    #[token("bool")]
    TBool,

    #[token("tup")]
    TTup,

    #[token("void")]
    Void,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keywords() {
        let src = "if else then func match let record enum type while break continue import export true false";
        let mut lexer = Token::lexer(&src);

        assert_eq!(lexer.next(), Some(Ok(Token::If)));
        assert_eq!(lexer.next(), Some(Ok(Token::Else)));
        assert_eq!(lexer.next(), Some(Ok(Token::Then)));
        assert_eq!(lexer.next(), Some(Ok(Token::Func)));
        assert_eq!(lexer.next(), Some(Ok(Token::Match)));
        assert_eq!(lexer.next(), Some(Ok(Token::Let)));
        assert_eq!(lexer.next(), Some(Ok(Token::Record)));
        assert_eq!(lexer.next(), Some(Ok(Token::Enum)));
        assert_eq!(lexer.next(), Some(Ok(Token::Type)));
        assert_eq!(lexer.next(), Some(Ok(Token::While)));
        assert_eq!(lexer.next(), Some(Ok(Token::Break)));
        assert_eq!(lexer.next(), Some(Ok(Token::Continue)));
        assert_eq!(lexer.next(), Some(Ok(Token::Import)));
        assert_eq!(lexer.next(), Some(Ok(Token::Export)));
        assert_eq!(lexer.next(), Some(Ok(Token::True)));
        assert_eq!(lexer.next(), Some(Ok(Token::False)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn types() {
        let src = "str char byte tup int  float bool void";
        let mut lexer = Token::lexer(&src);

        assert_eq!(lexer.next(), Some(Ok(Token::TStr)));
        assert_eq!(lexer.next(), Some(Ok(Token::TChar)));
        assert_eq!(lexer.next(), Some(Ok(Token::TByte)));
        assert_eq!(lexer.next(), Some(Ok(Token::TTup)));
        assert_eq!(lexer.next(), Some(Ok(Token::TInt)));
        assert_eq!(lexer.next(), Some(Ok(Token::TFloat)));
        assert_eq!(lexer.next(), Some(Ok(Token::TBool)));
        assert_eq!(lexer.next(), Some(Ok(Token::Void)));
        assert_eq!(lexer.next(), None);
    }

    #[test]
    fn others() {
        let src = r#"
            _someId123 "Some String!!\n" 'c' 123 1.23e2
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
        assert_eq!(lexer.next(), Some(Ok(Token::Char('c'))));
        assert_eq!(lexer.next(), Some(Ok(Token::Int(123))));
        assert_eq!(lexer.next(), Some(Ok(Token::Float(1.23e2))));
        assert_eq!(lexer.next(), Some(Ok(Token::Ident("abc".to_string()))));
        assert_eq!(lexer.next(), None);
    }
}
