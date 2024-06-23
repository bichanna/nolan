use std::fmt::Debug;
use string_interner::symbol::SymbolU32;

use crate::error::{combine, SourcePath, Span, Spanned};
use crate::lexer::Token;
use crate::types::Type;

pub trait Node {
    fn get_span(&self) -> &Span;
    fn get_type(&self) -> &Type;
}

#[derive(Debug, Clone, PartialEq)]
pub struct IntLiteral {
    pub value: i64,
    pub span: Span,
    pub type_: Type,
}

impl Node for IntLiteral {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FloatLiteral {
    pub value: f64,
    pub span: Span,
    pub type_: Type,
}

impl Node for FloatLiteral {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StrLiteral {
    pub value: SymbolU32,
    pub span: Span,
    pub type_: Type,
}

impl Node for StrLiteral {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BoolLiteral {
    pub value: bool,
    pub span: Span,
    pub type_: Type,
}

impl Node for BoolLiteral {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct List {
    pub elements: Vec<Expr>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Tuple {
    pub values: Vec<Expr>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub name: SymbolU32,
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

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVarAccess {
    pub source: SymbolU32,
    pub variant: SymbolU32,
    pub type_: Type,
    pub span: Span,
}

impl Node for EnumVarAccess {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldAccess {
    pub source: SymbolU32,
    pub field: SymbolU32,
    pub span: Span,
    pub type_: Type,
}

impl Node for StructFieldAccess {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct ModAccess {
    pub module: SymbolU32,
    pub constant: SymbolU32,
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

#[derive(Debug, Clone, PartialEq)]
pub struct EnumDef {
    pub name: SymbolU32,
    pub variants: Vec<SymbolU32>,
    pub span: Span,
    pub type_: Type,
}

impl Node for EnumDef {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Expr,
    pub arguments: Vec<Expr>,
    pub span: Span,
    pub type_: Type,
}

impl Node for Call {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldDef {
    pub name: SymbolU32,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: SymbolU32,
    pub fields: Vec<StructFieldDef>,
    pub span: Span,
    pub type_: Type,
}

impl Node for StructDef {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInitArg {
    pub name: SymbolU32,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructInit {
    pub name: SymbolU32,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: SymbolU32,
    pub path: SourcePath,
    pub expressions: Vec<TopLevelExpr>,
    pub type_: Type,
}

impl Module {
    pub fn new(name: SymbolU32, path: SourcePath, span: Span) -> Self {
        Self { name, path, expressions: vec![], type_: Type::Named(span, name) }
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

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam {
    pub name: SymbolU32,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub parameters: Vec<FuncParam>,
    pub return_type: Type,
    pub body: Vec<Expr>,
    pub span: Span,
    pub type_: Type,
}

impl Node for Closure {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub rec: bool,
    pub name: SymbolU32,
    pub closure: Closure,
    pub span: Span,
}

impl Node for Func {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        self.closure.get_type()
    }
}

#[derive(Debug, Copy, Clone, PartialEq)]
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

impl From<&Token> for BinaryOpKind {
    fn from(value: &Token) -> Self {
        match value {
            Token::Plus => Self::Add,
            Token::Minus => Self::Sub,
            Token::Mul => Self::Mul,
            Token::Div => Self::Div,
            Token::Rem => Self::Rem,
            Token::GT => Self::GT,
            Token::LT => Self::LT,
            Token::GE => Self::GE,
            Token::LE => Self::LE,
            Token::DEq => Self::Eq,
            Token::NotEq => Self::NEq,
            Token::And => Self::And,
            Token::Or => Self::Or,
            _ => Self::Eq,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BinaryOp {
    pub kind: BinaryOpKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Binary {
    pub lhs: Expr,
    pub rhs: Expr,
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

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnaryOpKind {
    NegBool,
    NegNum,
}

impl From<&Token> for UnaryOpKind {
    fn from(value: &Token) -> Self {
        match value {
            Token::Not => Self::NegBool,
            Token::Minus => Self::NegNum,
            _ => Self::NegBool,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct UnaryOp {
    pub kind: UnaryOpKind,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Unary {
    pub rhs: Expr,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Group {
    pub expression: Expr,
    pub span: Span,
}

impl Node for Group {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        self.expression.get_type()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Expr,
    pub then: Vec<Expr>,
    pub else_: Vec<Expr>,
    pub span: Span,
    pub type_: Type,
}

impl Node for If {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct When {
    pub condition: Expr,
    pub then: Vec<Expr>,
    pub else_: Option<Vec<Expr>>,
    pub span: Span,
    pub type_: Type,
}

impl Node for When {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct AssignVar {
    pub left: Expr,
    pub value: Expr,
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

#[derive(Debug, Clone, PartialEq)]
pub struct DefVar {
    pub name: SymbolU32,
    pub value: Expr,
    pub type_: Type,
    pub span: Span,
}

impl Node for DefVar {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Index {
    pub source: Expr,
    pub index: Expr,
    pub span: Span,
    pub type_: Type,
}

impl Node for Index {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Import {
    pub module: SymbolU32,
    pub imports: Option<Vec<SymbolU32>>,
    pub span: Span,
    pub type_: Type,
}

impl Node for Import {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Export {
    pub symbols: Vec<Spanned<SymbolU32>>,
    pub span: Span,
    pub type_: Type,
}

impl Node for Export {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub condition: Expr,
    pub body: Vec<Expr>,
    pub span: Span,
    pub type_: Type,
}

impl Node for While {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Break {
    pub span: Span,
    pub type_: Type,
}

impl Node for Break {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub value: Expr,
    pub span: Span,
}

impl Node for Return {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        self.value.get_type()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Void {
    pub span: Span,
    pub type_: Type,
}

impl Node for Void {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &self.type_
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Int(Box<IntLiteral>),
    Float(Box<FloatLiteral>),
    Str(Box<StrLiteral>),
    Bool(Box<BoolLiteral>),
    List(Box<List>),
    Tuple(Box<Tuple>),
    Ident(Box<Ident>),
    EnumVarAccess(Box<EnumVarAccess>),
    StructFieldAccess(Box<StructFieldAccess>),
    ModAccess(Box<ModAccess>),
    Call(Box<Call>),
    StructInit(Box<StructInit>),
    Closure(Box<Closure>),
    Binary(Box<Binary>),
    Unary(Box<Unary>),
    Group(Box<Group>),
    If(Box<If>),
    When(Box<When>),
    AssignVar(Box<AssignVar>),
    DefVar(Box<DefVar>),
    Index(Box<Index>),
    While(Box<While>),
    Break(Box<Break>),
    Return(Box<Return>),
    Void(Box<Void>),
}

impl Expr {
    #[inline]
    pub fn binary(
        left: Self,
        binary_op: BinaryOp,
        right: Self,
        type_: Type,
    ) -> Self {
        let new_span = combine(left.get_span(), right.get_span());
        Self::Binary(Box::new(Binary {
            lhs: left,
            rhs: right,
            operator: binary_op,
            span: new_span,
            type_,
        }))
    }

    #[inline]
    pub fn unary(unary_op: UnaryOp, right: Self, type_: Type) -> Self {
        let new_span = combine(&unary_op.span, right.get_span());
        Self::Unary(Box::new(Unary {
            rhs: right,
            operator: unary_op,
            span: new_span,
            type_,
        }))
    }
}

impl Node for Expr {
    fn get_span(&self) -> &Span {
        match self {
            Self::Int(ref node) => node.get_span(),
            Self::Float(ref node) => node.get_span(),
            Self::Str(ref node) => node.get_span(),
            Self::Bool(ref node) => node.get_span(),
            Self::List(ref node) => node.get_span(),
            Self::Tuple(ref node) => node.get_span(),
            Self::Ident(ref node) => node.get_span(),
            Self::EnumVarAccess(ref node) => node.get_span(),
            Self::StructFieldAccess(ref node) => node.get_span(),
            Self::ModAccess(ref node) => node.get_span(),
            Self::Call(ref node) => node.get_span(),
            Self::StructInit(ref node) => node.get_span(),
            Self::Closure(ref node) => node.get_span(),
            Self::Binary(ref node) => node.get_span(),
            Self::Unary(ref node) => node.get_span(),
            Self::Group(ref node) => node.get_span(),
            Self::If(ref node) => node.get_span(),
            Self::When(ref node) => node.get_span(),
            Self::AssignVar(ref node) => node.get_span(),
            Self::DefVar(ref node) => node.get_span(),
            Self::Index(ref node) => node.get_span(),
            Self::While(ref node) => node.get_span(),
            Self::Break(ref node) => node.get_span(),
            Self::Return(ref node) => node.get_span(),
            Self::Void(ref node) => node.get_span(),
        }
    }

    fn get_type(&self) -> &Type {
        match self {
            Self::Int(ref expr) => expr.get_type(),
            Self::Float(ref node) => node.get_type(),
            Self::Str(ref node) => node.get_type(),
            Self::Bool(ref node) => node.get_type(),
            Self::List(ref node) => node.get_type(),
            Self::Tuple(ref node) => node.get_type(),
            Self::Ident(ref node) => node.get_type(),
            Self::EnumVarAccess(ref node) => node.get_type(),
            Self::StructFieldAccess(ref node) => node.get_type(),
            Self::ModAccess(ref node) => node.get_type(),
            Self::Call(ref node) => node.get_type(),
            Self::StructInit(ref node) => node.get_type(),
            Self::Closure(ref node) => node.get_type(),
            Self::Binary(ref node) => node.get_type(),
            Self::Unary(ref node) => node.get_type(),
            Self::Group(ref node) => node.get_type(),
            Self::If(ref node) => node.get_type(),
            Self::When(ref node) => node.get_type(),
            Self::AssignVar(ref node) => node.get_type(),
            Self::DefVar(ref node) => node.get_type(),
            Self::Index(ref node) => node.get_type(),
            Self::While(ref node) => node.get_type(),
            Self::Break(ref node) => node.get_type(),
            Self::Return(ref node) => node.get_type(),
            Self::Void(ref node) => node.get_type(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelExpr {
    EnumDef(Box<EnumDef>),
    StructDef(Box<StructDef>),
    Module(Box<Module>),
    Func(Box<Func>),
    Import(Box<Import>),
    Export(Box<Export>),
}

impl Node for TopLevelExpr {
    fn get_span(&self) -> &Span {
        match self {
            Self::EnumDef(ref node) => node.get_span(),
            Self::StructDef(ref node) => node.get_span(),
            Self::Module(ref node) => node.get_span(),
            Self::Func(ref node) => node.get_span(),
            Self::Import(ref node) => node.get_span(),
            Self::Export(ref node) => node.get_span(),
        }
    }

    fn get_type(&self) -> &Type {
        match self {
            Self::EnumDef(ref node) => node.get_type(),
            Self::StructDef(ref node) => node.get_type(),
            Self::Module(ref node) => node.get_type(),
            Self::Func(ref node) => node.get_type(),
            Self::Import(ref node) => node.get_type(),
            Self::Export(ref node) => node.get_type(),
        }
    }
}
