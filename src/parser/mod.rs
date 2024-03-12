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
    fn statement(&mut self) -> Result<StmtNode, ParseError> {
        todo!()
    }

    fn var_declaration(&mut self) -> Result<StmtNode, ParseError> {
        self.next();

        let Token::Ident(name) = expect!(
            self,
            Token::Ident(..),
            ParseErrorType::ExpectedDiffTokenError("an identifier".to_string())
        ) else {
            unreachable!()
        };

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

        todo!() // TODO: Fix this after writing expression function
    }

    fn declaration(&mut self) -> Result<StmtNode, ParseError> {
        let current = current_token!(self);
        match current {
            Token::Let => self.var_declaration(),
            Token::Func => todo!(),
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

    // ---------- SIMPLIFICATION FUNCTIONS ----------

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
                let func_arg_type = if matches_this!(self, Token::ThreeDot) {
                    FuncArgType::VarArgs
                } else if matches_this!(self, Token::Eq) {
                    FuncArgType::Default(unimplemented!()) // TODO: Fix this later after writing expression function.
                } else {
                    FuncArgType::Positional
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
