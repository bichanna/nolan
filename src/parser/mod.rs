pub mod ast;
pub mod error;

use std::fs;

use crate::error::report_error;
use crate::lexer::error::LexError;
use crate::lexer::Token;
use crate::parser::ast::{ExprNode, StmtNode};
pub use crate::parser::error::{ParseError, ParseErrorType};
use crate::typechecker::types::{FuncArg, FuncArgType};
use crate::typechecker::{Type, TypeExpr};
use logos::{Lexer, Logos};

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
    fn expression(&mut self) -> Result<ExprNode, ParseError> {
        todo!()
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
                Ok(Token::Eq) => unimplemented!(), // TODO: implement this later
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
        let body = self.expression()?; // TODO: After expression func.

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
                    Ok(Token::Eq) => unimplemented!(), // TODO: implement this later
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
