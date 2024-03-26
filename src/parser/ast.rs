use crate::typechecker::TypeExpr;
use logos::Span;

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Div,
    Mul,
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

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum UnaryOp {
    NegBool,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Str(String),
    Char(char),
    Int(i64),
    Float(f64),
    Bool(bool),
    List(Vec<ExprNode>),
    Tup(Vec<ExprNode>),
}

/// All possible expression nodes.
/// TODO: Add pattern matching, record init, enum later.
#[derive(Clone, Debug, PartialEq)]
pub enum ExprNode {
    /// An identifier.
    Ident(String, Span),

    /// Literal expression.
    Literal(Literal, Span),

    /// Binary operation.
    Binary(TypeExpr, Box<Self>, BinaryOp, Box<Self>, Span),

    /// Unary operation.
    Unary(TypeExpr, UnaryOp, Box<Self>, Span),

    /// Grouping expression.
    Group(Box<Self>, Span),

    /// A block of expressions. The last expression in the block is the overall value of the type and the return value of the whole block.
    Block(TypeExpr, Vec<StmtNode>, Span),

    /// if-then-else expression.
    If(TypeExpr, Box<Self>, Box<Self>, Box<Self>, Span),

    /// A function without a name but with a body.
    Func(TypeExpr, Box<StmtNode>, Span),

    /// Apply a function with give arguments.
    Apply(TypeExpr, Box<Self>, Vec<Self>, Span),

    /// Index a list or tuple of with an expression that should evaluate to an integer.
    Index(TypeExpr, Box<Self>, Box<Self>, Span),

    /// Reassignment
    Assign(Box<Self>, Box<Self>, Span),
}

/// All possible statement nodes.
/// TODO: enum, record, import, export, type later
#[derive(Debug, Clone, PartialEq)]
pub enum StmtNode {
    /// Expression.
    Expr(ExprNode),

    /// A block.
    Block(TypeExpr, Vec<StmtNode>, Span),

    /// Assignment operation.
    Assign(TypeExpr, String, ExprNode, Span),

    /// While loop.
    While(ExprNode, Box<StmtNode>, Span),

    /// If-then-else statement.
    If(ExprNode, Box<StmtNode>, Option<Box<StmtNode>>, Span),

    /// A break statement.
    Break(Span),

    /// A continue statement.
    Continue(Span),

    /// A return statement.
    Return(Option<ExprNode>, Span),

    /// A function statement.
    Func(String, ExprNode, Span),
}
