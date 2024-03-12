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
