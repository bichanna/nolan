use std::fmt::Display;

use logos::Span;

use crate::lexer::Token;
use crate::parser::ast::ExprNode;

/// TODO: Add custom types such as record and enum later.
#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Integer,
    Float,
    String,
    Char,
    Byte,
    Bool,
    Void,
    List(Box<TypeExpr>),               // []type
    Tuple(Vec<TypeExpr>),              // tup(type...)
    Func(Vec<FuncArg>, Box<TypeExpr>), // \(type...): type
    Unknown,
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeExpr(pub Type, pub Option<Span>);

impl Type {
    pub fn from(value: &Token) -> Option<Self> {
        match value {
            Token::TInt => Some(Self::Integer),
            Token::TFloat => Some(Self::Float),
            Token::TStr => Some(Self::String),
            Token::TChar => Some(Self::Char),
            Token::TByte => Some(Self::Byte),
            Token::TBool => Some(Self::Bool),
            Token::Void => Some(Self::Void),
            _ => None,
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let type_str = match self {
            Type::Integer => "int".to_string(),
            Type::Float => "float".to_string(),
            Type::String => "str".to_string(),
            Type::Char => "char".to_string(),
            Type::Byte => "byte".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Void => "void".to_string(),
            Type::List(t) => format!("[]{}", t),
            Type::Tuple(ts) => format!(
                "tup({})",
                ts.iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            Type::Func(args, rt) => format!(
                "\\({}){}",
                args.iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", "),
                if let Type::Void = rt.0 {
                    "".to_string()
                } else {
                    format!(": {}", rt.0)
                }
            ),
            Type::Unknown => unimplemented!(),
        };

        write!(f, "{}", type_str)
    }
}

impl Display for TypeExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncArg {
    pub name: Option<String>,
    pub type_expr: TypeExpr,
    pub arg_type: FuncArgType,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FuncArgType {
    Positional,
    VarArgs,
    Default(ExprNode),
}

impl FuncArg {
    pub fn new(type_expr: TypeExpr, arg_type: FuncArgType) -> Self {
        Self {
            name: None,
            type_expr,
            arg_type,
        }
    }

    pub fn new_with_name(name: String, type_expr: TypeExpr, arg_type: FuncArgType) -> Self {
        Self {
            name: Some(name),
            type_expr,
            arg_type,
        }
    }
}

impl Display for FuncArg {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.type_expr, self.arg_type)
    }
}

impl Display for FuncArgType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let res = match self {
            Self::Positional => "",
            Self::VarArgs => "...",
            Self::Default(_) => "=",
        };

        write!(f, "{}", res)
    }
}
