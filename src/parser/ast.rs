use crate::typechecker::TypeExpr;

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

/// All possible expression nodes.
/// TODO: Add pattern matching, record init, enum later.
#[derive(Clone, Debug, PartialEq)]
pub enum ExprNode {
    /// Binary operation.
    Binary(TypeExpr, Box<Self>, BinaryOp, Box<Self>),

    /// Unary operation.
    Unary(TypeExpr, UnaryOp, Box<Self>),

    /// Grouping expression.
    Group(TypeExpr, Box<Self>),

    /// A block of expressions. The last expression in the block is the overall value of the type and the return value of the whole block.
    Block(TypeExpr, Vec<StmtNode>, Box<Self>),

    /// if-then-else expression.
    If(TypeExpr, Box<Self>, Box<Self>, Box<Self>),

    /// A function without a name but with a body.
    Func(TypeExpr, Box<Self>),

    /// Apply a function with give arguments.
    Apply(TypeExpr, Box<Self>, Vec<Self>),

    /// Index a list of with an expression that should evaluate to an integer.
    Index(TypeExpr, Box<Self>, Box<Self>),

    /// Get a specific value indexed by an integer in a tuple.
    TupGet(TypeExpr, Box<Self>, Box<Self>),
}

/// All possible statement nodes.
/// TODO: enum, record, import, export, type later
#[derive(Debug, Clone, PartialEq)]
pub enum StmtNode {
    /// Assignment operation. Returns void.
    Assign(TypeExpr, String, ExprNode),

    /// While loop.
    While(ExprNode, Box<Self>),

    /// If-then-else statement.
    If(TypeExpr, ExprNode, Box<Self>, Option<Box<Self>>),

    /// A break statement.
    Break,

    /// A continue statement.
    Continue,

    /// A return statement.
    Return(Option<ExprNode>),

    /// A function statement.
    Func(String, Box<Self>),
}
