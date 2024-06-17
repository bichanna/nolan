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
    Unknown(Span),
}

impl Type {
    pub fn ignore_span_equal(&self, other: &Type) -> bool {
        match self {
            Type::Int(_) => matches!(other, Type::Int(_)),
            Type::Float(_) => matches!(other, Type::Float(_)),
            Type::Str(_) => matches!(other, Type::Str(_)),
            Type::Bool(_) => matches!(other, Type::Bool(_)),
            Type::Void(_) => matches!(other, Type::Void(_)),
            Type::List(_, self_type) => {
                if let Type::List(_, other_type) = other {
                    self_type.ignore_span_equal(other_type)
                } else {
                    false
                }
            }
            Type::Tup(_, self_types) => {
                if let Type::Tup(_, other_types) = other {
                    self_types
                        .iter()
                        .zip(other_types)
                        .map(|(x, y)| x.ignore_span_equal(y))
                        .all(|x| x)
                } else {
                    false
                }
            }
            Type::Func(_, params, rt) => {
                if let Type::Func(_, other_params, other_rt) = other {
                    if !params
                        .iter()
                        .zip(other_params)
                        .map(|(x, y)| x.ignore_span_equal(y))
                        .all(|x| x)
                    {
                        return false;
                    }

                    rt.ignore_span_equal(other_rt)
                } else {
                    false
                }
            }
            Type::Named(_, typ) => {
                if let Type::Named(_, other_type) = other {
                    typ == other_type
                } else {
                    false
                }
            }
            Type::Unknown(_) => true,
        }
    }

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
            Self::Unknown(span) => span,
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
            Type::Unknown(_) => unimplemented!(),
        }
    }
}
