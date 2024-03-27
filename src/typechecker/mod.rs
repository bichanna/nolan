pub mod error;
pub mod types;

use std::fs;

use logos::Span;

use crate::error::report_error;
use crate::parser::ast::{BinaryOp, ExprNode, Literal, StmtNode, UnaryOp};
use crate::typechecker::error::{SemanticError, SemanticErrorType};
pub use crate::typechecker::types::{Type, TypeExpr};

macro_rules! throw_error {
    ($type: expr, $span: expr) => {{
        Err(SemanticError::new($type, $span.clone()))
    }};
}

type CheckResult<T> = Result<T, SemanticError>;

#[derive(Default)]
enum FuncType<'a> {
    #[default]
    None,
    Func(&'a TypeExpr),
}

struct Local<'a> {
    name: &'a String,
    depth: u32,
    type_expr: &'a TypeExpr,
}

impl<'a> Local<'a> {
    fn new(name: &'a String, depth: u32, type_expr: &'a TypeExpr) -> Self {
        Self {
            name,
            depth,
            type_expr,
        }
    }
}

struct Checker<'a> {
    locals: Vec<Local<'a>>,
    func_type: FuncType<'a>,
    scope_depth: u32,
}

pub fn check(filename: &str, ast: &mut Vec<StmtNode>) {
    let mut checker = Checker::new();
    checker.check(filename, ast);
}

impl<'a> Checker<'a> {
    fn new() -> Self {
        Self {
            locals: Vec::new(),
            func_type: FuncType::default(),
            scope_depth: 0,
        }
    }

    fn check(&mut self, filename: &str, ast: &mut Vec<StmtNode>) {
        for node in ast {
            if let Err(err) = self.check_stmt(node) {
                let src =
                    fs::read_to_string(filename).expect(&format!("No file named '{}'", filename));
                if let Err(err) = report_error(src, filename, err.to_string(), err.span) {
                    eprintln!("Failed error reporting: {}", err);
                }
            }
        }
    }

    fn check_stmt(&mut self, stmt: &mut StmtNode) -> CheckResult<Type> {
        match stmt {
            StmtNode::Expr(expr) => Ok(self.check_expr(expr)?),
            StmtNode::Block(t, block, span) => Ok(self.stmt_block(t, block, span)?),
            StmtNode::Assign(t, name, value, span) => Ok(self.stmt_assign(t, name, value, span)?),
            StmtNode::While(cond, body, span) => Ok(self.stmt_while(cond, body, span)?),
            StmtNode::If(cond, then_body, els, span) => {
                Ok(self.stmt_if(cond, then_body, els, span)?)
            }
            StmtNode::Break(span) => Ok(self.stmt_break(span)?),
            StmtNode::Continue(span) => Ok(self.stmt_continue(span)?),
            StmtNode::Return(expr, span) => Ok(self.stmt_return(expr, span)?),
            StmtNode::Func(name, func, span) => Ok(self.stmt_func(name, func, span)?),
        }
    }

    fn check_expr(&mut self, expr: &mut ExprNode) -> CheckResult<Type> {
        match expr {
            ExprNode::Ident(name, span) => Ok(self.expr_ident(name, span)?),
            ExprNode::Literal(literal, _) => Ok(self.expr_literal(literal)?),
            ExprNode::Binary(t, left, op, right, span) => {
                Ok(self.expr_binary(t, left, op, right, span)?)
            }
            ExprNode::Unary(t, op, right, span) => Ok(self.expr_unary(t, op, right, span)?),
            ExprNode::Group(expr, _) => Ok(self.check_expr(expr)?),
            ExprNode::Block(t, block, span) => Ok(self.expr_block(t, block, span)?),
            ExprNode::If(t, cond, then, els, span) => Ok(self.expr_if(t, cond, then, els, span)?),
            ExprNode::Func(t, body, span) => Ok(self.expr_func(t, body, span)?),
            ExprNode::Apply(t, callee, args, span) => Ok(self.expr_apply(t, callee, args, span)?),
            ExprNode::Index(t, indexed, index, span) => {
                Ok(self.expr_index(t, indexed, index, span)?)
            }
            ExprNode::Assign(left, right, span) => Ok(self.expr_assign(left, right, span)?),
        }
    }

    fn expr_ident(&mut self, name: &String, span: &Span) -> CheckResult<Type> {
        let local = self.resolve_local(name, span)?;
        Ok(local.type_expr.0.clone())
    }

    fn expr_literal(&mut self, literal: &mut Literal) -> CheckResult<Type> {
        match literal {
            Literal::Str(_) => Ok(Type::String),
            Literal::Char(_) => Ok(Type::Char),
            Literal::Int(_) => Ok(Type::Integer),
            Literal::Float(_) => Ok(Type::Float),
            Literal::Bool(_) => Ok(Type::Bool),
            Literal::List(expr) => {
                if let Some(expr) = expr.first_mut() {
                    let t = self.check_expr(expr)?;
                    Ok(Type::List(Box::new(TypeExpr(t, None))))
                } else {
                    Ok(Type::Unknown)
                }
            }
            Literal::Tup(exprs) => {
                let mut types = Vec::new();
                for expr in exprs {
                    types.push(TypeExpr(self.check_expr(expr)?, None));
                }
                Ok(Type::Tuple(types))
            }
        }
    }

    fn expr_binary(
        &mut self,
        t: &mut TypeExpr,
        left: &mut ExprNode,
        op: &BinaryOp,
        right: &mut ExprNode,
        span: &Span,
    ) -> CheckResult<Type> {
        let left = self.check_expr(left)?;
        let right = self.check_expr(right)?;

        match op {
            BinaryOp::Add => {
                if matches!(left, Type::Integer) && matches!(right, Type::Integer) {
                    *t = TypeExpr(Type::Integer, t.1.clone());
                    Ok(Type::Integer)
                } else if matches!(left, Type::Integer | Type::Float)
                    && matches!(right, Type::Integer | Type::Float)
                {
                    *t = TypeExpr(Type::Float, t.1.clone());
                    Ok(Type::Float)
                } else if matches!(left, Type::String) && matches!(right, Type::String) {
                    *t = TypeExpr(Type::String, t.1.clone());
                    Ok(Type::String)
                } else {
                    if matches!(left, Type::Float | Type::Integer) {
                        throw_error!(
                            SemanticErrorType::UnexpectedTypeError(Type::Float, right),
                            span
                        )
                    } else if matches!(left, Type::String) {
                        throw_error!(
                            SemanticErrorType::UnexpectedTypeError(Type::String, right),
                            span
                        )
                    } else {
                        throw_error!(SemanticErrorType::InvalidTypeError(right), span)
                    }
                }
            }
            BinaryOp::Sub => {
                if matches!(left, Type::Integer) && matches!(right, Type::Integer) {
                    *t = TypeExpr(Type::Integer, t.1.clone());
                    Ok(Type::Integer)
                } else if matches!(left, Type::Integer | Type::Float)
                    && matches!(right, Type::Integer | Type::Float)
                {
                    *t = TypeExpr(Type::Float, t.1.clone());
                    Ok(Type::Float)
                } else {
                    if matches!(left, Type::Float | Type::Integer) {
                        throw_error!(
                            SemanticErrorType::UnexpectedTypeError(Type::Float, right),
                            span
                        )
                    } else {
                        throw_error!(SemanticErrorType::InvalidTypeError(right), span)
                    }
                }
            }
            BinaryOp::Div => {
                if matches!(left, Type::Integer | Type::Float)
                    && matches!(right, Type::Integer | Type::Float)
                {
                    *t = TypeExpr(Type::Float, t.1.clone());
                    Ok(Type::Float)
                } else {
                    if matches!(left, Type::Float | Type::Integer) {
                        throw_error!(
                            SemanticErrorType::UnexpectedTypeError(Type::Float, right),
                            span
                        )
                    } else {
                        throw_error!(SemanticErrorType::InvalidTypeError(right), span)
                    }
                }
            }
            BinaryOp::Mul => {
                if matches!(left, Type::Integer) && matches!(right, Type::Integer) {
                    *t = TypeExpr(Type::Integer, t.1.clone());
                    Ok(Type::Integer)
                } else if matches!(left, Type::Integer | Type::Float)
                    && matches!(right, Type::Integer | Type::Float)
                {
                    *t = TypeExpr(Type::Float, t.1.clone());
                    Ok(Type::Float)
                } else {
                    if matches!(left, Type::Float | Type::Integer) {
                        throw_error!(
                            SemanticErrorType::UnexpectedTypeError(Type::Float, right),
                            span
                        )
                    } else {
                        throw_error!(SemanticErrorType::InvalidTypeError(right), span)
                    }
                }
            }
            BinaryOp::Rem => {
                if matches!(left, Type::Integer) && matches!(right, Type::Integer) {
                    *t = TypeExpr(Type::Integer, t.1.clone());
                    Ok(Type::Integer)
                } else if matches!(left, Type::Integer | Type::Float)
                    && matches!(right, Type::Integer | Type::Float)
                {
                    *t = TypeExpr(Type::Float, t.1.clone());
                    Ok(Type::Float)
                } else {
                    if matches!(left, Type::Float | Type::Integer) {
                        throw_error!(
                            SemanticErrorType::UnexpectedTypeError(Type::Float, right),
                            span
                        )
                    } else {
                        throw_error!(SemanticErrorType::InvalidTypeError(right), span)
                    }
                }
            }
            BinaryOp::GT => {
                if matches!(left, Type::Bool) && matches!(right, Type::Bool) {
                    *t = TypeExpr(Type::Bool, t.1.clone());
                    Ok(Type::Bool)
                } else {
                    throw_error!(SemanticErrorType::InvalidTypeError(right), span)
                }
            }
            BinaryOp::LT => {
                if matches!(left, Type::Bool) && matches!(right, Type::Bool) {
                    *t = TypeExpr(Type::Bool, t.1.clone());
                    Ok(Type::Bool)
                } else {
                    throw_error!(SemanticErrorType::InvalidTypeError(right), span)
                }
            }
            BinaryOp::GE => {
                if matches!(left, Type::Bool) && matches!(right, Type::Bool) {
                    *t = TypeExpr(Type::Bool, t.1.clone());
                    Ok(Type::Bool)
                } else {
                    throw_error!(SemanticErrorType::InvalidTypeError(right), span)
                }
            }
            BinaryOp::LE => {
                if matches!(left, Type::Bool) && matches!(right, Type::Bool) {
                    *t = TypeExpr(Type::Bool, t.1.clone());
                    Ok(Type::Bool)
                } else {
                    throw_error!(SemanticErrorType::InvalidTypeError(right), span)
                }
            }
            BinaryOp::Eq | BinaryOp::NEq => {
                if left == right {
                    *t = TypeExpr(Type::Bool, t.1.clone());
                    Ok(left)
                } else {
                    throw_error!(SemanticErrorType::InvalidTypeError(right), span)
                }
            }
            BinaryOp::And => {
                if matches!(left, Type::Bool) && matches!(right, Type::Bool) {
                    *t = TypeExpr(Type::Bool, t.1.clone());
                    Ok(Type::Bool)
                } else {
                    throw_error!(SemanticErrorType::InvalidTypeError(right), span)
                }
            }
            BinaryOp::Or => {
                if matches!(left, Type::Bool) && matches!(right, Type::Bool) {
                    *t = TypeExpr(Type::Bool, t.1.clone());
                    Ok(Type::Bool)
                } else {
                    throw_error!(SemanticErrorType::InvalidTypeError(right), span)
                }
            }
        }
    }

    fn expr_unary(
        &mut self,
        t: &mut TypeExpr,
        op: &UnaryOp,
        right: &mut ExprNode,
        span: &Span,
    ) -> CheckResult<Type> {
        let right = self.check_expr(right)?;
        *t = TypeExpr(Type::Bool, t.1.clone());
        match op {
            UnaryOp::NegBool => {
                if matches!(right, Type::Bool) {
                    Ok(Type::Bool)
                } else {
                    throw_error!(
                        SemanticErrorType::UnexpectedTypeError(Type::Bool, right),
                        span
                    )
                }
            }
        }
    }

    fn expr_block(
        &mut self,
        t: &mut TypeExpr,
        block: &mut Vec<StmtNode>,
        span: &Span,
    ) -> CheckResult<Type> {
        let typ = if let Some(mut last) = block.pop() {
            self.check_stmt(&mut last)?
        } else {
            Type::Void
        };

        for node in block {
            self.check_stmt(node)?;
        }

        *t = TypeExpr(typ.clone(), t.1.clone());

        Ok(typ)
    }

    fn expr_func(
        &mut self,
        t: &mut TypeExpr,
        body: &mut StmtNode,
        span: &Span,
    ) -> CheckResult<Type> {
        todo!()
    }

    fn expr_if(
        &mut self,
        t: &mut TypeExpr,
        cond: &mut ExprNode,
        then: &mut ExprNode,
        els: &mut ExprNode,
        span: &Span,
    ) -> CheckResult<Type> {
        todo!()
    }

    fn expr_apply(
        &mut self,
        t: &mut TypeExpr,
        callee: &mut ExprNode,
        args: &mut Vec<ExprNode>,
        span: &Span,
    ) -> CheckResult<Type> {
        todo!()
    }

    fn expr_index(
        &mut self,
        t: &mut TypeExpr,
        indexed: &mut ExprNode,
        index: &mut ExprNode,
        span: &Span,
    ) -> CheckResult<Type> {
        todo!()
    }

    fn expr_assign(
        &mut self,
        left: &mut ExprNode,
        right: &mut ExprNode,
        span: &Span,
    ) -> CheckResult<Type> {
        todo!()
    }

    fn stmt_block(
        &mut self,
        t: &mut TypeExpr,
        block: &mut Vec<StmtNode>,
        span: &Span,
    ) -> CheckResult<Type> {
        todo!()
    }

    fn stmt_assign(
        &mut self,
        t: &mut TypeExpr,
        name: &String,
        value: &mut ExprNode,
        span: &Span,
    ) -> CheckResult<Type> {
        todo!()
    }

    fn stmt_while(
        &mut self,
        cond: &mut ExprNode,
        body: &mut StmtNode,
        span: &Span,
    ) -> CheckResult<Type> {
        todo!()
    }

    fn stmt_if(
        &mut self,
        cond: &mut ExprNode,
        then_body: &mut StmtNode,
        els: &mut Option<Box<StmtNode>>,
        span: &Span,
    ) -> CheckResult<Type> {
        todo!()
    }

    fn stmt_break(&mut self, span: &Span) -> CheckResult<Type> {
        todo!()
    }

    fn stmt_continue(&mut self, span: &Span) -> CheckResult<Type> {
        todo!()
    }

    fn stmt_return(&mut self, expr: &mut Option<ExprNode>, span: &Span) -> CheckResult<Type> {
        todo!()
    }

    fn stmt_func(&mut self, name: &String, func: &mut ExprNode, span: &Span) -> CheckResult<Type> {
        todo!()
    }

    // -------------- HELPER FUNCTIONS --------------

    fn begin_scope(&mut self) {
        self.scope_depth += 1;
    }

    fn end_scope(&mut self) {
        self.scope_depth -= 1;
    }

    fn add_local(&mut self, t: &'a TypeExpr, name: &'a String) {
        self.locals.push(Local::new(name, self.scope_depth, t));
    }

    fn resolve_local(&self, name: &'a String, span: &Span) -> CheckResult<&Local> {
        if let Some(local) = self.locals.iter().rev().find(|local| local.name == name) {
            Ok(local)
        } else {
            throw_error!(SemanticErrorType::UndefinedError(name.clone()), span)
        }
    }
}
