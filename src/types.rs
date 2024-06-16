use string_interner::symbol::SymbolU32;
use string_interner::DefaultStringInterner;

use crate::error::Span;
use crate::lexer::Token;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Type {
    Int(Span),                        // int
    Float(Span),                      // float
    Str(Span),                        // str
    Bool(Span),                       // bool
    Void(Span),                       // void
    List(Span, Box<Type>),            // []`Type`
    Tup(Span, Vec<Type>),             // #(`Type`...)
    Func(Span, Vec<Type>, Box<Type>), // func(`Type`...) `Type`
    Named(Span, SymbolU32),
}

impl Type {
    pub fn get_span(&self) -> &Span {
        match self {
            Self::Int(span) => span,
            Self::Float(span) => span,
            Self::Str(span) => span,
            Self::Bool(span) => span,
            Self::Void(span) => span,
            Self::List(span, _) => span,
            Self::Tup(span, _) => span,
            Self::Func(span, _, _) => span,
            Self::Named(span, _) => span,
        }
    }

    pub fn convert_from(value: Token, span: Span) -> Option<Self> {
        match value {
            Token::TInt => Some(Type::Int(span)),
            Token::TFloat => Some(Type::Float(span)),
            Token::TStr => Some(Type::Str(span)),
            Token::TBool => Some(Type::Bool(span)),
            Token::Void => Some(Type::Void(span)),
            _ => None,
        }
    }

    pub fn display(&self, interner: &DefaultStringInterner) -> String {
        match self {
            Type::Int(_) => "int".to_string(),
            Type::Float(_) => "float".to_string(),
            Type::Str(_) => "str".to_string(),
            Type::Bool(_) => "bool".to_string(),
            Type::Void(_) => "void".to_string(),
            Type::List(_, t) => format!("[]{}", t.display(interner)),
            Type::Tup(_, ts) => format!(
                "#({})",
                ts.iter()
                    .map(|t| t.display(interner))
                    .collect::<Vec<String>>()
                    .join(",")
            ),
            Type::Func(_, args, rt) => format!(
                "func({}){}",
                args.iter()
                    .map(|t| t.display(interner))
                    .collect::<Vec<String>>()
                    .join(","),
                if let Type::Void(_) = **rt {
                    "".to_string()
                } else {
                    format!(": {}", rt.display(interner))
                }
            ),
            Type::Named(_, name) => {
                interner.resolve(*name).unwrap().to_string()
            }
        }
    }
}
