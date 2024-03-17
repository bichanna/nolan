pub mod ast;
pub mod error;

use std::fs;

use crate::error::report_error;
use crate::lexer::error::LexError;
use crate::lexer::Token;
use crate::parser::ast::{BinaryOp, ExprNode, StmtNode};
pub use crate::parser::error::{ParseError, ParseErrorType};
use crate::typechecker::types::{FuncArg, FuncArgType};
use crate::typechecker::{Type, TypeExpr};
use logos::{Lexer, Logos};

use self::ast::UnaryOp;

macro_rules! current_token {
    ($self: expr) => {{
        if let Err(lex_err) = &$self.current {
            return Err(ParseError::new(
                ParseErrorType::from(lex_err.clone()),
                $self.lexer.span(),
            ));
        }
        $self.current.as_ref().unwrap()
    }};
}

macro_rules! matches_this {
    ($self: expr, $token: pat) => {{
        matches!($self.current, Ok($token))
    }};
}

/// TODO: Create a short cut of this.
macro_rules! expect {
    ($self: expr, $token: pat, $err_type: expr) => {{
        if !matches!(current_token!($self), $token) {
            return Err(ParseError::new($err_type, $self.lexer.span()));
        } else {
            $self.next();
        }

        current_token!($self)
    }};
}

macro_rules! binary_assign {
    ($self: expr, $bin_op: expr, $expr: expr, $span: expr) => {{
        $self.next();
        Ok(ExprNode::Assign(
            Box::new($expr.clone()),
            Box::new(ExprNode::Binary(
                TypeExpr(Type::Unknown, None),
                Box::new($expr),
                $bin_op,
                Box::new($self.expression()?),
                $span.clone(),
            )),
            $span,
        ))
    }};
}

struct Parser<'a> {
    filename: &'a str,
    lexer: Lexer<'a, Token>,
    ast: Vec<StmtNode>,
    current: Result<Token, LexError>,
    done: bool,
    had_error: bool,
}

/// Parses the given source code to an AST.
pub fn parse(filename: &str) -> Option<Vec<StmtNode>> {
    let src = fs::read_to_string(filename).expect(&format!("No file named '{}'", filename));
    let mut lexer = Token::lexer(&src);

    let current = lexer.next();
    if let Some(current) = &current {
        if let Err(err) = current {
            if let Err(err) = report_error(src.clone(), filename, err.to_string(), lexer.span()) {
                eprintln!("Failed error reporting: {}", err);
            }
            std::process::exit(1);
        }
    } else {
        return None;
    }

    let current = current.unwrap();
    let mut parser = Parser {
        lexer,
        filename,
        current,
        ast: vec![],
        done: false,
        had_error: false,
    };
    parser.parse();
    return Some(parser.ast);
}

impl Parser<'_> {
    fn call(&mut self, arg: Option<ExprNode>) -> Result<ExprNode, ParseError> {
        todo!()
    }

    fn unary(&mut self) -> Result<ExprNode, ParseError> {
        let span = self.lexer.span();
        if matches_this!(self, Token::Not) {
            Ok(ExprNode::Unary(
                TypeExpr(Type::Bool, Some(span.clone())),
                UnaryOp::NegBool,
                Box::new(self.unary()?),
                span,
            ))
        } else {
            Ok(self.call(None)?)
        }
    }

    fn factor(&mut self) -> Result<ExprNode, ParseError> {
        let mut expr = self.unary()?;

        while matches_this!(self, Token::Div)
            || matches_this!(self, Token::Mul)
            || matches_this!(self, Token::Rem)
        {
            let span = self.lexer.span();
            let bin_op = if matches_this!(self, Token::Div) {
                BinaryOp::Div
            } else if matches_this!(self, Token::Rem) {
                BinaryOp::Rem
            } else {
                BinaryOp::Mul
            };
            expr = ExprNode::Binary(
                TypeExpr(Type::Bool, None),
                Box::new(expr),
                bin_op,
                Box::new(self.unary()?),
                span,
            );
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<ExprNode, ParseError> {
        let mut expr = self.factor()?;

        while matches_this!(self, Token::Minus) || matches_this!(self, Token::Plus) {
            let span = self.lexer.span();
            let bin_op = if matches_this!(self, Token::Minus) {
                BinaryOp::Sub
            } else {
                BinaryOp::Add
            };
            expr = ExprNode::Binary(
                TypeExpr(Type::Bool, None),
                Box::new(expr),
                bin_op,
                Box::new(self.factor()?),
                span,
            );
        }

        Ok(expr)
    }

    fn comp_expr(&mut self) -> Result<ExprNode, ParseError> {
        let mut expr = self.term()?;

        while matches_this!(self, Token::GT)
            || matches_this!(self, Token::LT)
            || matches_this!(self, Token::GE)
            || matches_this!(self, Token::LE)
        {
            let span = self.lexer.span();
            let bin_op = if matches_this!(self, Token::GT) {
                BinaryOp::GT
            } else if matches_this!(self, Token::LT) {
                BinaryOp::LT
            } else if matches_this!(self, Token::GE) {
                BinaryOp::GE
            } else {
                BinaryOp::LE
            };
            expr = ExprNode::Binary(
                TypeExpr(Type::Bool, None),
                Box::new(expr),
                bin_op,
                Box::new(self.term()?),
                span,
            );
        }

        Ok(expr)
    }

    fn eq_expr(&mut self) -> Result<ExprNode, ParseError> {
        let mut expr = self.comp_expr()?;

        while matches_this!(self, Token::DEq) || matches_this!(self, Token::NotEq) {
            let span = self.lexer.span();
            let bin_op = if matches_this!(self, Token::DEq) {
                BinaryOp::Eq
            } else {
                BinaryOp::NEq
            };
            expr = ExprNode::Binary(
                TypeExpr(Type::Bool, None),
                Box::new(expr),
                bin_op,
                Box::new(self.comp_expr()?),
                span,
            );
        }

        Ok(expr)
    }

    fn and_expr(&mut self) -> Result<ExprNode, ParseError> {
        let mut expr = self.eq_expr()?;

        while matches_this!(self, Token::And) {
            let span = self.lexer.span();
            expr = ExprNode::Binary(
                TypeExpr(Type::Bool, None),
                Box::new(expr),
                BinaryOp::And,
                Box::new(self.eq_expr()?),
                span,
            );
        }

        Ok(expr)
    }

    fn or_expr(&mut self) -> Result<ExprNode, ParseError> {
        let mut expr = self.and_expr()?;

        while matches_this!(self, Token::Or) {
            let span = self.lexer.span();
            expr = ExprNode::Binary(
                TypeExpr(Type::Bool, None),
                Box::new(expr),
                BinaryOp::Or,
                Box::new(self.and_expr()?),
                span,
            );
        }

        Ok(expr)
    }

    fn assignment(&mut self) -> Result<ExprNode, ParseError> {
        let expr = self.or_expr()?;
        let span = self.lexer.span();

        match &self.current {
            Ok(Token::Eq) => {
                self.next();
                Ok(ExprNode::Assign(
                    Box::new(expr),
                    Box::new(self.expression()?),
                    span,
                ))
            }
            Ok(Token::PlusEq) => binary_assign!(self, BinaryOp::Add, expr, span),
            Ok(Token::MinusEq) => binary_assign!(self, BinaryOp::Sub, expr, span),
            Ok(Token::MulEq) => binary_assign!(self, BinaryOp::Mul, expr, span),
            Ok(Token::DivEq) => binary_assign!(self, BinaryOp::Div, expr, span),
            Ok(Token::RemEq) => binary_assign!(self, BinaryOp::Rem, expr, span),
            Err(err) => Err(ParseError::new(ParseErrorType::LexError(err.clone()), span)),
            _ => Ok(expr),
        }
    }

    fn expression(&mut self) -> Result<ExprNode, ParseError> {
        self.assignment()
    }

    fn expr_statement(&mut self) -> Result<StmtNode, ParseError> {
        let expr = self.expression()?;
        Ok(StmtNode::Expr(expr))
    }
    fn statement(&mut self) -> Result<StmtNode, ParseError> {
        let span = self.lexer.span();

        match self.current {
            Ok(Token::While) => {
                self.next();

                let cond = self.expression()?;

                expect!(
                    self,
                    Token::Then,
                    ParseErrorType::ExpectedDiffTokenError("'then'".to_string())
                );

                let body = self.expression()?;

                Ok(StmtNode::While(cond, body, span))
            }
            Ok(Token::If) => {
                self.next();

                let cond = self.expression()?;

                expect!(
                    self,
                    Token::Then,
                    ParseErrorType::ExpectedDiffTokenError("'then'".to_string())
                );

                let then_body = self.expression()?;

                let else_body = if matches_this!(self, Token::Else) {
                    self.next();
                    Some(self.expression()?)
                } else {
                    None
                };

                Ok(StmtNode::If(cond, then_body, else_body, span))
            }
            Ok(Token::Continue) => {
                self.next();
                Ok(StmtNode::Continue(span))
            }
            Ok(Token::Break) => {
                self.next();
                Ok(StmtNode::Break(span))
            }
            _ => self.expr_statement(),
        }
    }

    fn var_declaration(&mut self) -> Result<StmtNode, ParseError> {
        let span = self.lexer.span();

        self.next();

        let Token::Ident(name) = expect!(
            self,
            Token::Ident(..),
            ParseErrorType::ExpectedDiffTokenError("an identifier".to_string())
        ) else {
            unreachable!()
        };

        let name = name.clone();

        let var_type = if matches_this!(self, Token::Colon) {
            self.next();
            self.get_type_expr()?
        } else {
            TypeExpr(Type::Unknown, None)
        };

        expect!(
            self,
            Token::Eq,
            ParseErrorType::ExpectedDiffTokenError("'='".to_string())
        );

        let value = self.expression()?;

        Ok(StmtNode::Assign(var_type, name, value, span))
    }

    fn func_declaration(&mut self) -> Result<StmtNode, ParseError> {
        let span = self.lexer.span();

        self.next();

        let Token::Ident(name) = expect!(
            self,
            Token::Ident(..),
            ParseErrorType::ExpectedDiffTokenError("an identifier".to_string())
        ) else {
            unreachable!()
        };
        let name = name.clone();

        // TODO: Genetic check here.

        expect!(
            self,
            Token::LeftParen,
            ParseErrorType::ExpectedDiffTokenError("(".to_string())
        );

        // Get parameters.
        let mut params = Vec::<FuncArg>::new();
        while !matches_this!(self, Token::RightParen) {
            let Token::Ident(param_name) = expect!(
                self,
                Token::Ident(..),
                ParseErrorType::ExpectedDiffTokenError("an identifier".to_string())
            ) else {
                unreachable!()
            };
            let param_name = param_name.clone();

            expect!(
                self,
                Token::Colon,
                ParseErrorType::ExpectedDiffTokenError(":".to_string())
            );

            let type_expr = self.get_type_expr()?;

            let param_type = match self.current {
                Ok(Token::Eq) => FuncArgType::Default(self.expression()?),
                Ok(Token::ThreeDot) => FuncArgType::VarArgs,
                _ => FuncArgType::Positional,
            };

            let param = FuncArg::new_with_name(param_name, type_expr, param_type);
            params.push(param);

            if !matches_this!(self, Token::Comma) {
                break;
            } else {
                self.next();
            }
        }

        expect!(
            self,
            Token::RightParen,
            ParseErrorType::ExpectedDiffTokenError(")".to_string())
        );

        // Get return type.
        let return_type = if matches_this!(self, Token::Colon) {
            self.next();
            self.get_type_expr()?
        } else {
            let result = TypeExpr(Type::Void, Some(self.lexer.span()));
            self.next();
            result
        };

        // Get body.
        let body_span = self.lexer.span();
        let body = self.expression()?;

        let func = StmtNode::Func(
            name,
            ExprNode::Func(return_type, Box::new(body), body_span),
            span,
        );

        Ok(func)
    }

    fn declaration(&mut self) -> Result<StmtNode, ParseError> {
        let current = current_token!(self);
        match current {
            Token::Let => self.var_declaration(),
            Token::Func => self.func_declaration(),
            _ => {
                let result = self.statement();

                expect!(
                    self,
                    Token::NewLine,
                    ParseErrorType::ExpectedDiffTokenError(
                        "Expected a new line or ';'".to_string()
                    )
                );

                return result;
            }
        }
    }

    fn get_type_expr(&mut self) -> Result<TypeExpr, ParseError> {
        if matches_this!(self, Token::LeftBrak) {
            // List
            self.next();
            expect!(
                self,
                Token::RightBrak,
                ParseErrorType::InvalidTypeError("expected ]".to_string())
            );

            let inner_type = self.get_type_expr()?;
            Ok(TypeExpr(
                Type::List(Box::new(inner_type)),
                Some(self.lexer.span()),
            ))
        } else if matches_this!(self, Token::TTup) {
            // Tuple
            self.next();
            expect!(
                self,
                Token::LeftParen,
                ParseErrorType::InvalidTypeError("expected (".to_string())
            );

            let mut types = Vec::<TypeExpr>::new();
            while !matches_this!(self, Token::RightParen) {
                let inner_type = self.get_type_expr()?;
                types.push(inner_type);

                if !matches_this!(self, Token::Comma) {
                    break;
                } else {
                    self.next();
                }
            }

            expect!(
                self,
                Token::RightParen,
                ParseErrorType::InvalidTypeError("expected )".to_string())
            );

            Ok(TypeExpr(Type::Tuple(types), Some(self.lexer.span())))
        } else if matches_this!(self, Token::BackSlash) {
            // Func
            self.next();
            expect!(
                self,
                Token::LeftParen,
                ParseErrorType::InvalidTypeError("expected (".to_string())
            );

            let mut args = Vec::<FuncArg>::new();
            while !matches_this!(self, Token::RightParen) {
                let type_expr = self.get_type_expr()?;

                let func_arg_type = match self.current {
                    Ok(Token::Eq) => FuncArgType::Default(self.expression()?),
                    Ok(Token::ThreeDot) => FuncArgType::VarArgs,
                    _ => FuncArgType::Positional,
                };

                args.push(FuncArg::new(type_expr, func_arg_type));

                if !matches_this!(self, Token::Comma) {
                    break;
                } else {
                    self.next();
                }
            }

            expect!(
                self,
                Token::RightParen,
                ParseErrorType::InvalidTypeError("expected )".to_string())
            );

            let mut type_span = None;
            let return_type = if matches_this!(self, Token::Colon) {
                self.next();
                type_span = Some(self.lexer.span());
                self.get_type_expr()?
            } else {
                TypeExpr(Type::Unknown, None)
            };

            Ok(TypeExpr(Type::Func(args, Box::new(return_type)), type_span))
        } else {
            let current = current_token!(self);
            if let Some(t) = Type::from(current) {
                Ok(TypeExpr(t, Some(self.lexer.span())))
            } else {
                Err(ParseError::new(
                    ParseErrorType::InvalidTypeError(format!("{:?}", current_token!(self))),
                    self.lexer.span(),
                ))
            }
        }
    }

    fn parse(&mut self) {
        while !self.done {
            let parse_result = self.declaration();
            if let Ok(node) = parse_result {
                self.ast.push(node);
            } else {
                self.had_error = true;

                let Err(err) = parse_result else {
                    unreachable!()
                };
                let filename = self.filename;
                let src =
                    fs::read_to_string(filename).expect(&format!("No file named '{}'", filename));
                if let Err(err) = report_error(src, filename, err.to_string(), err.span) {
                    eprintln!("Failed error reporting: {}", err);
                }
            }
        }
    }

    // -------------- HELPER FUNCTIONS --------------

    fn peek(&mut self) -> Option<Token> {
        let mut peekable = self.lexer.by_ref().peekable();
        if let Some(result) = peekable.peek() {
            if let Ok(token) = result {
                return Some(token.clone());
            }
        }

        None
    }

    fn next(&mut self) {
        let token = self.lexer.next();
        if let None = token {
            self.done = true;
        } else {
            self.current = token.unwrap();
        }
    }
}
