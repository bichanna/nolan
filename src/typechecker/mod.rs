pub mod error;
pub mod types;

use logos::Span;

use crate::parser::ast::{BinaryOp, ExprNode, StmtNode, UnaryOp};
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

pub fn check(ast: &Vec<StmtNode>) {
    let mut checker = Checker::new();
    checker.check(ast);
}

impl<'a> Checker<'a> {
    fn new() -> Self {
        Self {
            locals: Vec::new(),
            func_type: FuncType::default(),
            scope_depth: 0,
        }
    }

    fn check(&mut self, ast: &Vec<StmtNode>) {
        for node in ast {
            let res = self.check_stmt(node);
        }
    }

    fn check_stmt(&mut self, stmt: &StmtNode) -> CheckResult<()> {
        match stmt {
            StmtNode::Expr(expr) => self.check_expr(expr)?,
            StmtNode::Block(t, block, _) => {
                for node in block {
                    self.check_stmt(node)?;
                }
            }
            StmtNode::Assign(t, name, value, span) => self.stmt_assign(t, name, value, span)?,
            StmtNode::While(cond, body, span) => self.stmt_while(cond, body, span)?,
            StmtNode::If(cond, then_body, els, span) => self.stmt_if(cond, then_body, els, span)?,
            StmtNode::Break(span) => self.stmt_break(span)?,
            StmtNode::Continue(span) => self.stmt_continue(span)?,
            StmtNode::Return(expr, span) => self.stmt_return(expr, span)?,
            StmtNode::Func(name, func, span) => self.stmt_func(name, func, span)?,
        }
        Ok(())
    }

    fn check_expr(&mut self, expr: &ExprNode) -> CheckResult<()> {
        match expr {
            ExprNode::Ident(name, span) => self.expr_ident(name, span)?,
            ExprNode::Literal(_, _) => {}
            ExprNode::Binary(t, left, op, right, span) => {
                self.expr_binary(t, left, op, right, span)?
            }
            ExprNode::Unary(t, op, right, span) => self.expr_unary(t, op, right, span)?,
            ExprNode::Group(expr, _) => self.check_expr(expr)?,
            ExprNode::Block(t, block, span) => self.expr_block(t, block, span)?,
            ExprNode::If(t, cond, then, els, span) => self.expr_if(t, cond, then, els, span)?,
            ExprNode::Func(t, body, span) => self.expr_func(t, body, span)?,
            ExprNode::Apply(t, callee, args, span) => self.expr_apply(t, callee, args, span)?,
            ExprNode::Index(t, indexed, index, span) => self.expr_index(t, indexed, index, span)?,
            ExprNode::Assign(left, right, span) => self.expr_assign(left, right, span)?,
        }
        Ok(())
    }

    fn expr_ident(&mut self, name: &String, span: &Span) -> CheckResult<()> {
        todo!()
    }

    fn expr_binary(
        &mut self,
        t: &TypeExpr,
        left: &ExprNode,
        op: &BinaryOp,
        right: &ExprNode,
        span: &Span,
    ) -> CheckResult<()> {
        todo!()
    }

    fn expr_unary(
        &mut self,
        t: &TypeExpr,
        op: &UnaryOp,
        right: &ExprNode,
        span: &Span,
    ) -> CheckResult<()> {
        todo!()
    }

    fn expr_block(&mut self, t: &TypeExpr, block: &Vec<StmtNode>, span: &Span) -> CheckResult<()> {
        todo!()
    }

    fn expr_func(&mut self, t: &TypeExpr, body: &StmtNode, span: &Span) -> CheckResult<()> {
        todo!()
    }

    fn expr_if(
        &mut self,
        t: &TypeExpr,
        cond: &ExprNode,
        then: &ExprNode,
        els: &ExprNode,
        span: &Span,
    ) -> CheckResult<()> {
        todo!()
    }

    fn expr_apply(
        &mut self,
        t: &TypeExpr,
        callee: &ExprNode,
        args: &Vec<ExprNode>,
        span: &Span,
    ) -> CheckResult<()> {
        todo!()
    }

    fn expr_index(
        &mut self,
        t: &TypeExpr,
        indexed: &ExprNode,
        index: &ExprNode,
        span: &Span,
    ) -> CheckResult<()> {
        todo!()
    }

    fn expr_assign(&mut self, left: &ExprNode, right: &ExprNode, span: &Span) -> CheckResult<()> {
        todo!()
    }

    fn stmt_assign(
        &mut self,
        t: &TypeExpr,
        name: &String,
        value: &ExprNode,
        span: &Span,
    ) -> CheckResult<()> {
        todo!()
    }

    fn stmt_while(&mut self, cond: &ExprNode, body: &StmtNode, span: &Span) -> CheckResult<()> {
        todo!()
    }

    fn stmt_if(
        &mut self,
        cond: &ExprNode,
        then_body: &StmtNode,
        els: &Option<Box<StmtNode>>,
        span: &Span,
    ) -> CheckResult<()> {
        todo!()
    }

    fn stmt_break(&mut self, span: &Span) -> CheckResult<()> {
        todo!()
    }

    fn stmt_continue(&mut self, span: &Span) -> CheckResult<()> {
        todo!()
    }

    fn stmt_return(&mut self, expr: &Option<ExprNode>, span: &Span) -> CheckResult<()> {
        todo!()
    }

    fn stmt_func(&mut self, name: &String, func: &ExprNode, span: &Span) -> CheckResult<()> {
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
