use crate::error::Spanned;
use crate::parser::pattern::Pattern;
use crate::types::{SpannedType, Type};

#[derive(Clone, Debug)]
pub enum BinaryOp {
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

#[derive(Clone, Debug)]
pub enum UnaryOp {
    NegBool,
    NegNum,
}

#[derive(Clone, Debug)]
pub enum Literal {
    /// Identifier.
    Ident(String),
    /// String literal.
    Str(String),

    /// Character literal.
    Char(char),

    /// Integer literal.
    Int(i64),

    /// Float literal.
    Float(f64),

    /// Bool literal.
    Bool(bool),

    /// List literal.
    List(Vec<Node>),
}

pub type SpannedNode = Spanned<Node>;

#[derive(Clone, Debug)]
pub enum Node {
    Literal(Literal),

    /// Function expression.
    Func {
        params: Vec<(String, SpannedType)>,
        return_type: SpannedType,
    },

    /// Named function.
    FuncDef {
        name: String,
        func: Box<Self>,
    },

    /// Binary expression.
    Binary {
        lhs: Box<Self>,
        op: BinaryOp,
        rhs: Box<Self>,
        type_: Type,
    },

    /// Unary expression.
    Unary {
        op: UnaryOp,
        rhs: Box<Self>,
        type_: Type,
    },

    /// Grouping.
    Group(Box<Self>),

    /// A block of expressions. The last expression is the overall type of the block.
    Block {
        block: Vec<Self>,
        last: Box<Self>,
        type_: Type,
    },

    /// If-then-else expression.
    If {
        cond: Box<Self>,
        then: (Vec<Self>, Box<Self>),
        else_: Box<Self>,
        type_: Type,
    },

    /// When flow.
    When {
        cond: Box<Self>,
        then: (Vec<Self>, Box<Self>),
        else_: Option<Box<Self>>,
    },

    /// Call a function with the given arguments, or initialize a data var with the given argument.
    Apply {
        callee: Box<Self>,
        args: Vec<Self>,
        type_: Type,
    },

    /// Index operation.
    Index {
        indexed: Box<Self>,
        index: Box<Self>,
        type_: Type,
    },

    /// Variable definition.
    DefAssign {
        lhs: Box<Pattern>,
        rhs: Box<Self>,
        type_: SpannedType,
    },

    /// Assigning to variable.
    Assign {
        lhs: Box<Pattern>,
        rhs: Box<Self>,
        type_: SpannedType,
    },

    /// Use statement.
    Use {
        lib: String,
        import: Option<Vec<String>>,
    },

    /// Accessing module or record object.
    Access {
        obj: Box<Self>,
        access: String,
        type_: Type,
    },

    /// Accessing data variant.
    DataGet {
        data: Type,
        var: String,
    },

    /// Data definition
    DataDef {
        name: String,
        vars: Vec<(String, Option<SpannedType>)>,
    },

    /// Record definition.
    RecordDef {
        name: String,
        props: Vec<(String, SpannedType)>,
    },

    Match {
        cond: Box<Self>,
        arms: Vec<(Pattern, Box<Self>)>,
        type_: Type,
    },
}
