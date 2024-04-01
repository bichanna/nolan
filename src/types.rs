use std::fmt::Display;

use crate::error::Spanned;
use crate::lexer::Token;

pub type SpannedType = Spanned<Type>;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Int,                                      // int
    Float,                                    // float
    Str,                                      // str
    Char,                                     // char
    Bool,                                     // bool
    Void,                                     // void
    List(Box<SpannedType>),                   // []`Type`
    Tup(Vec<SpannedType>),                    // <`Type`...>
    Func(Vec<SpannedType>, Box<SpannedType>), // func(`Type`...): `Type`
    Named(String),
    Unknown,
}

impl TryFrom<Token> for Type {
    type Error = ();

    fn try_from(value: Token) -> Result<Self, Self::Error> {
        match value {
            Token::TInt => Ok(Type::Int),
            Token::TFloat => Ok(Type::Float),
            Token::TStr => Ok(Type::Str),
            Token::TChar => Ok(Type::Char),
            Token::TBool => Ok(Type::Bool),
            Token::Void => Ok(Type::Void),
            _ => Err(()),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            Type::Int => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::Str => "str".to_string(),
            Type::Char => "char".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Void => "void".to_string(),
            Type::List(t) => format!("[]{t}"),
            Type::Tup(ts) => format!(
                "<{}>",
                ts.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Type::Func(args, rt) => format!(
                "func({}){}",
                args.iter()
                    .map(|t| t.to_string())
                    .collect::<Vec<String>>()
                    .join(","),
                if let Type::Void = rt.0 {
                    "".to_string()
                } else {
                    format!(": {}", rt)
                }
            ),
            Type::Named(name) => format!("{name}"),
            Type::Unknown => unimplemented!(),
        };

        write!(f, "{}", res)
    }
}

impl Display for SpannedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
