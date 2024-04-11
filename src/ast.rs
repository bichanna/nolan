use std::fmt::Debug;

use crate::error::{combine, SourcePath, Span, Spanned};
use crate::types::{SpannedType, Type};

pub trait Node {
    fn get_span(&self) -> &Span;
    fn get_type(&self) -> &Type;
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct EnumVarDef {
    pub name: String,
    pub types: Vec<SpannedType>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Call {
    pub callee: Expr,
    pub arguments: Vec<Expr>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct StructFieldDef {
    pub name: String,
    pub type_: SpannedType,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<StructFieldDef>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct StructInitArg {
    pub name: String,
    pub value: Expr,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
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

#[derive(Debug, Clone, PartialEq)]
pub struct Module {
    pub name: String,
    pub path: SourcePath,
    pub expressions: Vec<TopLevelExpr>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct FuncParam {
    pub name: String,
    pub type_: SpannedType,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    pub parameters: Vec<FuncParam>,
    pub return_type: SpannedType,
    pub body: Vec<Expr>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Func {
    pub pure: bool,
    pub rec: bool,
    pub name: String,
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

#[derive(Debug, Clone, PartialEq)]
pub struct If {
    pub condition: Expr,
    pub then: Expr,
    pub else_: Expr,
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

#[derive(Debug, Clone, PartialEq)]
pub struct When {
    pub condition: Expr,
    pub then: Expr,
    pub else_: Option<Expr>,
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

#[derive(Debug, Clone, PartialEq)]
pub struct AssignVar {
    pub name: String,
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
    pub name: String,
    pub value: Expr,
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

#[derive(Debug, Clone, PartialEq)]
pub struct Index {
    pub source: Expr,
    pub index: Expr,
    pub type_: Type,
    pub span: Span,
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
pub struct Use {
    pub module: String,
    pub imports: Option<Vec<String>>,
    pub span: Span,
}

impl Node for Use {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &Type::Void
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Export {
    pub symbols: Vec<Spanned<String>>,
    pub span: Span,
}

impl Node for Export {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &Type::Void
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct While {
    pub condition: Expr,
    pub body: Vec<Expr>,
    pub span: Span,
}

impl Node for While {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &Type::Void
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Break {
    pub span: Span,
}

impl Node for Break {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        &Type::Void
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub value: Option<Expr>,
    pub span: Span,
}

impl Node for Return {
    fn get_span(&self) -> &Span {
        &self.span
    }

    fn get_type(&self) -> &Type {
        if let Some(ref value) = self.value {
            value.get_type()
        } else {
            &Type::Void
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum EnumVarPattern {
    VarAccess(Box<EnumVarAccess>),
    /// `callee` of `Box<Call>` should be `EnumVarAccess`
    VarInit(Box<Call>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct PropertyPattern {
    pub name: String,
    pub pattern: Pattern,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct StructPattern {
    pub properties: Vec<PropertyPattern>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ListPattern {
    pub elements: Vec<Pattern>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct TuplePattern {
    pub values: Vec<Pattern>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct OrPattern {
    pub patterns: Vec<Pattern>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Pattern {
    Ident(Box<Ident>),
    Variant(Box<EnumVarPattern>),
    Struct(Box<StructPattern>),
    Int(Box<IntLiteral>),
    Float(Box<FloatLiteral>),
    Str(Box<StrLiteral>),
    Bool(Box<BoolLiteral>),
    List(Box<ListPattern>),
    Tuple(Box<TuplePattern>),
    Wildcard(Span),
    Or(Box<OrPattern>),
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchCase {
    pub pattern: Pattern,
    pub guard: Option<Expr>,
    pub body: Vec<Expr>,
    pub type_: Type,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Match {
    pub expression: Expr,
    pub expressions: Vec<MatchCase>,
    pub type_: Type,
    pub span: Span,
}

impl Node for Match {
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
    StructAccess(Box<StructAccess>),
    ModAccess(Box<ModAccess>),
    EnumDef(Box<EnumDef>),
    Call(Box<Call>),
    StructDef(Box<StructDef>),
    StructInit(Box<StructInit>),
    Module(Box<Module>),
    Closure(Box<Closure>),
    Func(Box<Func>),
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
}

impl Expr {
    #[inline]
    pub fn binary(
        left: Self,
        binary_op: BinaryOp,
        right: Self,
        type_: Option<Type>,
    ) -> Self {
        let new_span = combine(left.get_span(), right.get_span());
        Self::Binary(Box::new(Binary {
            lhs: left,
            rhs: right,
            operator: binary_op,
            span: new_span,
            type_: if let Some(type_) = type_ { type_ } else { Type::Unknown },
        }))
    }

    #[inline]
    pub fn unary(unary_op: UnaryOp, right: Self, type_: Option<Type>) -> Self {
        let new_span = combine(&unary_op.span, right.get_span());
        Self::Unary(Box::new(Unary {
            rhs: right,
            operator: unary_op,
            span: new_span,
            type_: if let Some(type_) = type_ { type_ } else { Type::Unknown },
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
            Self::StructAccess(ref node) => node.get_span(),
            Self::ModAccess(ref node) => node.get_span(),
            Self::EnumDef(ref node) => node.get_span(),
            Self::Call(ref node) => node.get_span(),
            Self::StructDef(ref node) => node.get_span(),
            Self::StructInit(ref node) => node.get_span(),
            Self::Module(ref node) => node.get_span(),
            Self::Closure(ref node) => node.get_span(),
            Self::Func(ref node) => node.get_span(),
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
            Self::StructAccess(ref node) => node.get_type(),
            Self::ModAccess(ref node) => node.get_type(),
            Self::EnumDef(ref node) => node.get_type(),
            Self::Call(ref node) => node.get_type(),
            Self::StructDef(ref node) => node.get_type(),
            Self::StructInit(ref node) => node.get_type(),
            Self::Module(ref node) => node.get_type(),
            Self::Closure(ref node) => node.get_type(),
            Self::Func(ref node) => node.get_type(),
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
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum TopLevelExpr {
    EnumDef(Box<EnumDef>),
    StructDef(Box<StructDef>),
    Module(Box<Module>),
    Func(Box<Func>),
    Use(Box<Use>),
    Export(Box<Export>),
}

impl Node for TopLevelExpr {
    fn get_span(&self) -> &Span {
        match self {
            Self::EnumDef(ref node) => node.get_span(),
            Self::StructDef(ref node) => node.get_span(),
            Self::Module(ref node) => node.get_span(),
            Self::Func(ref node) => node.get_span(),
            Self::Use(ref node) => node.get_span(),
            Self::Export(ref node) => node.get_span(),
        }
    }

    fn get_type(&self) -> &Type {
        match self {
            Self::EnumDef(ref node) => node.get_type(),
            Self::StructDef(ref node) => node.get_type(),
            Self::Module(ref node) => node.get_type(),
            Self::Func(ref node) => node.get_type(),
            Self::Use(ref node) => node.get_type(),
            Self::Export(ref node) => node.get_type(),
        }
    }
}
