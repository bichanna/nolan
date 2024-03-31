use std::path::Path;
use std::rc::Rc;

use crate::error::{SourcePath, Span};
use crate::types::{SpannedType, Type};

pub trait Node {
    fn get_span(&self) -> &Span;
    fn get_type(&self) -> &Type;
}

pub struct IntLiteral {
    pub value: i64,
    pub span: Span,
}

impl Node for IntLiteral {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &Type::Int
    }
}

pub struct FloatLiteral {
    pub value: f64,
    pub span: Span,
}

impl Node for FloatLiteral {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &Type::Float
    }
}

pub struct CharLiteral {
    pub value: char,
    pub span: Span,
}

impl Node for CharLiteral {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &Type::Char
    }
}

pub struct StrLiteral {
    pub value: String,
    pub span: Span,
}

impl Node for StrLiteral {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &Type::Str
    }
}

pub struct BoolLiteral {
    pub value: bool,
    pub span: Span,
}

impl Node for BoolLiteral {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &Type::Bool
    }
}

pub struct List {
    pub elements: Vec<Box<dyn Node>>, // TODO: change to Expr
    pub span: Span,
    pub type_: Type,
}

impl Node for List {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

pub struct Tuple {
    pub values: Vec<Box<dyn Node>>, // TODO: change to Expr
    pub span: Span,
    pub type_: Type,
}

impl Node for Tuple {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

pub struct Ident {
    pub name: String,
    pub span: Span,
    pub type_: Type,
}

impl Node for Ident {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

pub struct EnumVarAccess {
    pub source: String,
    pub variant: String,
    pub span: Span,
    pub type_: Type,
}

impl Node for EnumVarAccess {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

pub struct StructAccess {
    pub source: String,
    pub property: String,
    pub span: Span,
    pub type_: Type,
}

impl Node for StructAccess {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

pub struct ModAccess {
    pub module: String,
    pub constant: String,
    pub span: Span,
    pub type_: Type,
}

impl Node for ModAccess {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

pub struct EnumVarDef {
    pub name: String,
    pub types: Vec<SpannedType>,
    pub span: Span,
}

pub struct EnumDef {
    pub name: String,
    pub variants: Vec<EnumVarDef>,
    pub span: Span,
}

impl Node for EnumDef {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &Type::Unknown
    }
}

pub struct Call {
    pub callee: Box<dyn Node>, // TODO: change to Expr
    pub arguments: Vec<Box<dyn Node>>, // TODO: change to Expr
    pub type_: Type,
    pub span: Span,
}

impl Node for Call {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

pub struct StructPropDef {
    pub name: String,
    pub type_: SpannedType,
    pub span: Span,
}

pub struct StructDef {
    pub name: String,
    pub properties: Vec<StructPropDef>,
    pub span: Span,
}

impl Node for StructDef {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &Type::Unknown
    }
}

pub struct StructInitArg {
    pub name: String,
    pub value: Box<dyn Node>, // TODO: change to Expr
    pub span: Span,
}

pub struct StructInit {
    pub name: String,
    pub arguments: Vec<StructInitArg>,
    pub type_: Type,
    pub span: Span,
}

impl Node for StructInit {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

pub struct Module {
    pub name: String,
    pub path: Rc<Path>,
    pub expressions: Vec<Box<dyn Node>>, // TODO: change to TopLevelExpr
    pub type_: Type,
}

impl Module {
    pub fn new(name: String, path: SourcePath) -> Self {
        Self {
            name: name.clone(),
            path,
            expressions: vec![],
            type_: Type::Named(name),
        }
    }
}

impl Node for Module {
    fn get_span(&self) -> &Span {
        unimplemented!()
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

pub struct FuncParam {
    pub name: String,
    pub type_: SpannedType,
    pub span: Span,
}

pub struct Closure {
    pub parameters: Vec<FuncParam>,
    pub return_type: SpannedType,
    pub type_: Type,
    pub span: Span,
}

impl Node for Closure {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

pub struct Func {
    pub name: String,
    pub closure: Closure,
    pub span: Span,
}

impl Node for Func {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.closure.get_type()
    }
}

pub enum BinaryOpKind {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    GT,
    LT,
    GE,
    LE,
    Eq,
    NEq,
    And,
    Or,
}

pub struct BinaryOp {
    pub kind: BinaryOpKind,
    pub span: Span,
}

pub struct Binary {
    pub lhs: Box<dyn Node>, // TODO: change to Expr
    pub rhs: Box<dyn Node>, // TODO: change to Expr
    pub operator: BinaryOp,
    pub span: Span,
    pub type_: Type,
}

impl Node for Binary {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

pub enum UnaryOpKind {
    NegBool,
    NegNum,
}

pub struct UnaryOp {
    pub kind: UnaryOpKind,
    pub span: Span,
}

pub struct Unary {
    pub rhs: Box<dyn Node>, // TODO: change to Expr
    pub operator: UnaryOp,
    pub span: Span,
    pub type_: Type,
}

impl Node for Unary {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

pub struct Group {
    pub expression: Box<dyn Node>, // TODO: change to Expr
    pub type_: Type,
    pub span: Span,
}

impl Node for Group {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

pub struct If {
    pub condition: Box<dyn Node>, // TODO: change to Expr
    pub then: Box<dyn Node>,      // TODO: change to Expr
    pub else_: Box<dyn Node>,     // TODO: change to Expr
    pub type_: Type,
    pub span: Span,
}

impl Node for If {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

pub struct When {
    pub condition: Box<dyn Node>, // TODO: change to Expr
    pub then: Box<dyn Node>,      // TODO: change to Expr
    pub else_: Option<Box<dyn Node>>, // TODO: change to Expr
    pub span: Span,
}

impl Node for When {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &Type::Void
    }
}

pub struct AssignVar {
    pub name: String,
    pub value: Box<dyn Node>, // TODO: change to Expr
    pub type_: Type,
    pub span: Span,
}

impl Node for AssignVar {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

pub struct DefVar {
    pub name: String,
    pub value: Box<dyn Node>, // TODO: change to Expr
    pub type_: SpannedType,
    pub span: Span,
}

impl Node for DefVar {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_.0
    }
}
