use crate::parser::ast::ExprNode;

/// TODO: Add custom types such as record and enum later.
#[derive(Clone, Debug, PartialEq)]
pub enum TypeExpr {
    Integer,
    Float,
    String,
    Char,
    Byte,
    Bool,
    Void,
    List(Box<TypeExpr>),
    Tuple(Vec<TypeExpr>),
    Func(Vec<FuncArg>, Box<TypeExpr>),
    Unknown,
}

#[derive(Clone, Debug, PartialEq)]
pub struct FuncArg {
    name: String,
    type_expr: TypeExpr,
    arg_type: FuncArgType,
}

#[derive(Clone, Debug, PartialEq)]
pub enum FuncArgType {
    Positional,
    VarArgs,
    Default(ExprNode),
}
