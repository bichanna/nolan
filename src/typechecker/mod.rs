mod scope;
pub mod types;

use std::collections::HashMap;

use logos::Span;

use crate::parser::ast::{BinaryOp, ExprNode, StmtNode, UnaryOp};
use crate::typechecker::scope::Scope;
pub use crate::typechecker::types::{Type, TypeExpr};

#[derive(Default)]
enum FuncType<'a> {
    #[default]
    None,
    Func(&'a TypeExpr),
}

struct Checker<'a> {
    scopes: Vec<Scope<'a>>,
    locals: HashMap<&'a String, (i32, &'a TypeExpr)>,
    func_type: FuncType<'a>,
}

pub fn check(ast: &Vec<StmtNode>) {
    let mut checker = Checker::new();
    checker.check(ast);
}

impl<'a> Checker<'a> {
    fn new() -> Self {
        Self {
            scopes: Vec::new(),
            locals: HashMap::new(),
            func_type: FuncType::default(),
        }
    }

    fn check(&mut self, ast: &Vec<StmtNode>) {
        for node in ast {
            self.check_stmt(node);
        }
    }

    fn check_stmt(&mut self, stmt: &StmtNode) {
        match stmt {
            StmtNode::Expr(expr) => self.check_expr(expr),
            StmtNode::Block(t, block, _) => {
                for node in block {
                    self.check_stmt(node);
                }
            }
            StmtNode::Assign(t, name, value, span) => self.stmt_assign(t, name, value, span),
            StmtNode::While(cond, body, span) => self.stmt_while(cond, body, span),
            StmtNode::If(cond, then_body, els, span) => self.stmt_if(cond, then_body, els, span),
            StmtNode::Break(_) => {}
            StmtNode::Continue(_) => {}
            StmtNode::Return(expr, span) => self.stmt_return(expr, span),
            StmtNode::Func(name, func, span) => self.stmt_func(name, func, span),
        }
    }

    fn check_expr(&mut self, expr: &ExprNode) {
        match expr {
            ExprNode::Ident(name, span) => self.expr_ident(name, span),
            ExprNode::Literal(_, _) => {}
            ExprNode::Binary(t, left, op, right, span) => {
                self.expr_binary(t, left, op, right, span)
            }
            ExprNode::Unary(t, op, right, span) => self.expr_unary(t, op, right, span),
            ExprNode::Group(expr, _) => self.check_expr(expr),
            ExprNode::Block(t, block, span) => self.expr_block(t, block, span),
            ExprNode::If(t, cond, then, els, span) => self.expr_if(t, cond, then, els, span),
            ExprNode::Func(t, body, span) => self.expr_func(t, body, span),
            ExprNode::Apply(t, callee, args, span) => self.expr_apply(t, callee, args, span),
            ExprNode::Index(t, indexed, index, span) => self.expr_index(t, indexed, index, span),
            ExprNode::Assign(left, right, span) => self.expr_assign(left, right, span),
        }
    }

    fn expr_ident(&mut self, name: &String, span: &Span) {
        todo!()
    }

    fn expr_binary(
        &mut self,
        t: &TypeExpr,
        left: &ExprNode,
        op: &BinaryOp,
        right: &ExprNode,
        span: &Span,
    ) {
        todo!()
    }

    fn expr_unary(&mut self, t: &TypeExpr, op: &UnaryOp, right: &ExprNode, span: &Span) {
        todo!()
    }

    fn expr_block(&mut self, t: &TypeExpr, block: &Vec<StmtNode>, span: &Span) {
        todo!()
    }

    fn expr_func(&mut self, t: &TypeExpr, body: &StmtNode, span: &Span) {
        todo!()
    }

    fn expr_if(
        &mut self,
        t: &TypeExpr,
        cond: &ExprNode,
        then: &ExprNode,
        els: &ExprNode,
        span: &Span,
    ) {
        todo!()
    }

    fn expr_apply(&mut self, t: &TypeExpr, callee: &ExprNode, args: &Vec<ExprNode>, span: &Span) {
        todo!()
    }

    fn expr_index(&mut self, t: &TypeExpr, indexed: &ExprNode, index: &ExprNode, span: &Span) {
        todo!()
    }

    fn expr_assign(&mut self, left: &ExprNode, right: &ExprNode, span: &Span) {
        todo!()
    }

    fn stmt_assign(&mut self, t: &TypeExpr, name: &String, value: &ExprNode, span: &Span) {
        todo!()
    }

    fn stmt_while(&mut self, cond: &ExprNode, body: &StmtNode, span: &Span) {
        todo!()
    }

    fn stmt_if(
        &mut self,
        cond: &ExprNode,
        then_body: &StmtNode,
        els: &Option<Box<StmtNode>>,
        span: &Span,
    ) {
        todo!()
    }

    fn stmt_return(&mut self, expr: &Option<ExprNode>, span: &Span) {
        todo!()
    }

    fn stmt_func(&mut self, name: &String, func: &ExprNode, span: &Span) {
        todo!()
    }
}
