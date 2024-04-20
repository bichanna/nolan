use logos::{Lexer, Logos};
use std::fs;

use crate::ast::*;
use crate::error::{combine, LexError, ParseError, ParseErrorKind, Span};
use crate::error::{SourcePath, Spanned};
use crate::lexer::Token;
use crate::types::{SpannedType, Type};

type ParseResult<T> = Result<T, ParseError>;

type ModParseResult<T> = Result<T, Vec<ParseError>>;

macro_rules! throw_error {
    ($span: expr, $msg: expr, $($f: expr),*) => {
        return Err(Spanned(ParseErrorKind::ParseErr(format!($msg, $($f),*)), $span))
    };

    ($span: expr, $msg: expr) => {
        return Err(Spanned(ParseErrorKind::ParseErr($msg.to_string()), $span))
    }
}

macro_rules! expect {
    ($current: expr, $expected: pat, $span: expr, $msg: expr, $($f: expr),*) => {{
        if !matches!($current, $expected) {
            throw_error!($span, $msg, $($f),*);
        }

        $current
    }};

    ($current: expr, $expected: pat, $span: expr, $msg: expr) => {{
        if !matches!($current, $expected) {
            throw_error!($span, $msg);
        }

        $current
    }};
}

/// A recursive-descent parser that converts tokenized Nolan source code into an AST.
struct Parser<'a> {
    lexer: Lexer<'a, Token>,
    ast: Vec<TopLevelExpr>,
    current: Result<Token, LexError>,
    errors: Vec<ParseError>,
    done: bool,
}

pub fn parse(source: SourcePath) -> ModParseResult<Module> {
    let module_name = source
        .0
        .file_stem()
        .unwrap()
        .to_str()
        .expect("invalid file stem")
        .to_string();
    let src = fs::read_to_string(source.clone())
        .unwrap_or_else(|_| panic!("Invalid path '{}'", source));

    let mut lexer = Token::lexer(&src);

    let current = lexer.next();

    if let Some(ref current) = current {
        if let Err(err) = current {
            return Err(vec![Spanned(
                ParseErrorKind::LexErr(err.clone()),
                lexer.span(),
            )]);
        }
    } else {
        return Ok(Module::new(module_name, source));
    }

    let exprs = Parser::new(lexer, current.unwrap()).parse();

    Ok(Module {
        name: module_name.clone(),
        path: source,
        expressions: exprs?,
        type_: Type::Named(module_name),
    })
}

#[cfg(test)]
pub fn test_parse(src: &str) -> ModParseResult<Vec<TopLevelExpr>> {
    let mut lexer = Token::lexer(src);

    let current = lexer.next();

    if let Some(ref current) = current {
        if let Err(err) = current {
            return Err(vec![Spanned(
                ParseErrorKind::LexErr(err.clone()),
                lexer.span(),
            )]);
        }
    } else {
        return Ok(vec![]);
    }

    Parser::new(lexer, current.unwrap()).parse()
}

impl<'a> Parser<'a> {
    fn new(lexer: Lexer<'a, Token>, current: Result<Token, LexError>) -> Self {
        Self {
            lexer,
            current,
            errors: Vec::new(),
            ast: Vec::new(),
            done: false,
        }
    }

    fn parse(mut self) -> ModParseResult<Vec<TopLevelExpr>> {
        while !self.done {
            // dbg!(&self.current);
            // dbg!(&self.done);

            let result = self.parse_top_level_expression();

            // dbg!(&result);

            if let Err(err) = result {
                self.errors.push(err);
            } else {
                self.ast.push(result.unwrap());
            }
        }

        if self.errors.is_empty() {
            Ok(self.ast)
        } else {
            Err(self.errors)
        }
    }

    #[inline]
    fn next(&mut self) {
        if let Some(token) = self.lexer.next() {
            self.current = token;
        } else {
            self.done = true;
            self.current = Err(LexError::WithMessage(
                "unexpected end of source".to_string(),
            ));
        }
    }

    #[inline]
    fn current(&self) -> Result<&Token, ParseError> {
        if let Err(ref err) = self.current {
            Err(Spanned(ParseErrorKind::LexErr(err.clone()), self.lexer.span()))
        } else {
            Ok(self.current.as_ref().unwrap())
        }
    }

    fn parse_top_level_expression(&mut self) -> ParseResult<TopLevelExpr> {
        let current = self.current()?;
        let span = self.lexer.span();

        match current {
            Token::Enum => self.parse_enum_def(),
            Token::Struct => self.parse_struct_def(),
            Token::Func | Token::Pure | Token::Rec => self.parse_func(),
            Token::Use => self.parse_use(),
            Token::Export => self.parse_export(),
            _ => {
                let current = current.clone();
                self.next();
                throw_error!(
                    span,
                    "expected a top-level expression but found '{:?}'",
                    current
                )
            }
        }
    }

    fn parse_list_type(&mut self) -> ParseResult<SpannedType> {
        let span = self.lexer.span();

        expect!(self.current()?, Token::LeftBrak, span, "expected '['");

        self.next();

        expect!(
            self.current()?,
            Token::RightBrak,
            self.lexer.span(),
            "expected ']'"
        );

        self.next();

        let inner_type = self.parse_type()?;

        Ok(Spanned(
            Type::List(Box::new(inner_type.0.clone())),
            combine(&span, &inner_type.1),
        ))
    }

    fn parse_tuple_type(&mut self) -> ParseResult<SpannedType> {
        let span = self.lexer.span();

        expect!(self.current()?, Token::LT, span, "expected '<'");

        self.next();

        let mut types = Vec::<Type>::new();
        while !matches!(self.current, Ok(Token::GT)) {
            types.push(self.parse_type()?.0);

            if !matches!(self.current, Ok(Token::Comma)) {
                break;
            } else {
                self.next();
            }
        }

        expect!(
            self.current()?,
            Token::GT,
            self.lexer.span(),
            "expected '>' after type expressions"
        );

        self.next();

        Ok(Spanned(Type::Tup(types), combine(&span, &self.lexer.span())))
    }

    fn parse_func_type(&mut self) -> ParseResult<SpannedType> {
        let span = self.lexer.span();

        expect!(self.current()?, Token::Func, span, "expected 'func'");

        self.next();

        expect!(
            self.current()?,
            Token::LeftParen,
            self.lexer.span(),
            "expected '('"
        );

        self.next();

        let mut args = Vec::<SpannedType>::new();
        while !matches!(self.current, Ok(Token::RightParen)) {
            args.push(self.parse_type()?);

            if !matches!(self.current, Ok(Token::Comma)) {
                break;
            } else {
                self.next();
            }
        }

        expect!(
            self.current()?,
            Token::RightParen,
            self.lexer.span(),
            "expected ')' after type expressions"
        );

        self.next();

        let return_type = if matches!(self.current, Ok(Token::Void)) {
            let span = self.lexer.span();
            let t = Type::try_from(self.current()?.clone()).unwrap();
            self.next();
            Spanned(t, span)
        } else {
            self.parse_type()?
        };

        let end_span = self.lexer.span();

        Ok(Spanned(
            Type::Func(args, Box::new(return_type)),
            combine(&span, &end_span),
        ))
    }

    fn parse_type(&mut self) -> ParseResult<SpannedType> {
        match self.current()? {
            Token::LeftBrak => self.parse_list_type(),
            Token::LT => self.parse_tuple_type(),
            Token::Func => self.parse_func_type(),
            _ => {
                let current = self.current()?.clone();
                let span = self.lexer.span();
                if let Ok(t) = Type::try_from(current) {
                    self.next();
                    Ok(Spanned(t, span))
                } else {
                    throw_error!(span, "invalid type information")
                }
            }
        }
    }

    fn parse_enum_var_def(&mut self) -> ParseResult<EnumVarDef> {
        let span = self.lexer.span();

        let Token::Ident(enum_var_name) = expect!(
            self.current()?,
            Token::Ident(..),
            self.lexer.span(),
            "expected an identifier"
        ) else {
            unreachable!()
        };

        let enum_var_name = enum_var_name.clone();

        self.next();

        let mut types = Vec::<SpannedType>::new();

        if matches!(self.current, Ok(Token::LeftParen)) {
            self.next();

            while !matches!(self.current, Ok(Token::RightParen)) {
                types.push(self.parse_type()?);
                if !matches!(self.current, Ok(Token::Comma)) {
                    break;
                } else {
                    self.next();
                }
            }

            expect!(
                self.current()?,
                Token::RightParen,
                self.lexer.span(),
                "expected ')' after type expressions"
            );

            self.next();
        }

        Ok(EnumVarDef {
            name: enum_var_name,
            types,
            span: combine(&span, &self.lexer.span()),
        })
    }

    fn parse_enum_def(&mut self) -> ParseResult<TopLevelExpr> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::Enum,
            self.lexer.span(),
            "expected 'enum'"
        );

        self.next();

        let Token::Ident(enum_name) = expect!(
            self.current()?,
            Token::Ident(..),
            self.lexer.span(),
            "expected an enum name"
        ) else {
            unreachable!()
        };

        let enum_name = enum_name.clone();

        self.next();

        expect!(
            self.current()?,
            Token::LeftBrace,
            self.lexer.span(),
            "expected '{'"
        );

        self.next();

        let mut enum_vars = Vec::<EnumVarDef>::new();
        while !matches!(self.current, Ok(Token::RightBrace)) {
            enum_vars.push(self.parse_enum_var_def()?);
            if !matches!(self.current, Ok(Token::Comma)) {
                break;
            } else {
                self.next();
            }
        }

        expect!(
            self.current()?,
            Token::RightBrace,
            self.lexer.span(),
            "expected '}' after enum variants"
        );

        self.next();

        Ok(TopLevelExpr::EnumDef(Box::new(EnumDef {
            name: enum_name,
            variants: enum_vars,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_struct_field_def(&mut self) -> ParseResult<StructFieldDef> {
        let span = self.lexer.span();

        let Token::Ident(field_name) = expect!(
            self.current()?,
            Token::Ident(..),
            self.lexer.span(),
            "expected an identifier for a field"
        ) else {
            unreachable!()
        };

        let field_name = field_name.clone();

        self.next();

        let field_type = self.parse_type()?;

        Ok(StructFieldDef {
            name: field_name,
            type_: field_type,
            span: combine(&span, &self.lexer.span()),
        })
    }

    fn parse_struct_def(&mut self) -> ParseResult<TopLevelExpr> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::Struct,
            self.lexer.span(),
            "expected 'struct'"
        );

        self.next();

        let Token::Ident(struct_name) = expect!(
            self.current()?,
            Token::Ident(..),
            self.lexer.span(),
            "expected an identifier for a struct name"
        ) else {
            unreachable!()
        };

        let struct_name = struct_name.clone();

        self.next();

        expect!(
            self.current()?,
            Token::LeftBrace,
            self.lexer.span(),
            "expected '{'"
        );

        self.next();

        let mut struct_fields = Vec::<StructFieldDef>::new();
        while !matches!(self.current, Ok(Token::RightBrace)) {
            struct_fields.push(self.parse_struct_field_def()?);
            if !matches!(self.current, Ok(Token::Comma)) {
                break;
            } else {
                self.next();
            }
        }

        expect!(
            self.current()?,
            Token::RightBrace,
            self.lexer.span(),
            "expected '}' after struct fields"
        );

        self.next();

        Ok(TopLevelExpr::StructDef(Box::new(StructDef {
            name: struct_name,
            fields: struct_fields,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_params(&mut self) -> ParseResult<Vec<FuncParam>> {
        expect!(
            self.current()?,
            Token::LeftParen,
            self.lexer.span(),
            "expected '('"
        );

        self.next();

        let mut params = Vec::<FuncParam>::new();

        while !matches!(self.current, Ok(Token::RightParen)) {
            let span = self.lexer.span();

            let Token::Ident(param_name) = expect!(
                self.current()?,
                Token::Ident(..),
                self.lexer.span(),
                "expected a parameter name"
            ) else {
                unreachable!()
            };

            let param_name = param_name.clone();

            self.next();

            let param_type = self.parse_type()?;

            let param = FuncParam {
                name: param_name,
                type_: param_type,
                span: combine(&span, &self.lexer.span()),
            };

            params.push(param);

            if !matches!(self.current, Ok(Token::Comma)) {
                break;
            } else {
                self.next();
            }
        }

        expect!(
            self.current()?,
            Token::RightParen,
            self.lexer.span(),
            "expected ')' after parameters"
        );

        self.next();

        Ok(params)
    }

    fn parse_curly_body(&mut self) -> ParseResult<Vec<Expr>> {
        expect!(
            self.current()?,
            Token::LeftBrace,
            self.lexer.span(),
            "expected '{' for the start of a function body"
        );

        self.next();

        let mut body = Vec::<Expr>::new();

        while !matches!(self.current, Ok(Token::RightBrace)) {
            body.push(self.parse_standalone_expression()?);
        }

        expect!(
            self.current()?,
            Token::RightBrace,
            self.lexer.span(),
            "expected '}' after function body"
        );

        self.next();

        Ok(body)
    }

    fn parse_func(&mut self) -> ParseResult<TopLevelExpr> {
        let span = self.lexer.span();

        let pure = if matches!(self.current, Ok(Token::Pure)) {
            self.next();
            true
        } else {
            false
        };

        let rec = if matches!(self.current, Ok(Token::Rec)) {
            self.next();
            true
        } else {
            false
        };

        expect!(
            self.current()?,
            Token::Func,
            self.lexer.span(),
            "expected 'func'"
        );

        self.next();

        let Token::Ident(func_name) = expect!(
            self.current()?,
            Token::Ident(..),
            self.lexer.span(),
            "expected an identifier for a function name"
        ) else {
            unreachable!()
        };

        let func_name = func_name.clone();

        self.next();

        let params = self.parse_params()?;

        let return_type = self.parse_type()?;

        let body = if matches!(self.current, Ok(Token::Do)) {
            self.next();
            vec![self.parse_standalone_expression()?]
        } else {
            self.parse_curly_body()?
        };

        let span = combine(&span, &self.lexer.span());

        Ok(TopLevelExpr::Func(Box::new(Func {
            pure,
            rec,
            name: func_name,
            closure: Closure {
                parameters: params,
                return_type,
                body,
                type_: Type::Unknown,
                span: span.clone(),
            },
            span,
        })))
    }

    fn parse_use(&mut self) -> ParseResult<TopLevelExpr> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::Use,
            self.lexer.span(),
            "expected 'use'"
        );

        self.next();

        let Token::Ident(mod_name) = expect!(
            self.current()?,
            Token::Ident(..),
            self.lexer.span(),
            "expected a module name"
        ) else {
            unreachable!()
        };

        let mod_name = mod_name.clone();

        self.next();

        let mut import_symbols: Option<Vec<String>> = None;

        if matches!(self.current, Ok(Token::LeftBrak)) {
            self.next();

            let mut imports = Vec::<String>::new();

            while !matches!(self.current, Ok(Token::RightBrak)) {
                let Token::Ident(import) = expect!(
                    self.current()?,
                    Token::Ident(..),
                    self.lexer.span(),
                    "expected an identifier"
                ) else {
                    unreachable!()
                };

                imports.push(import.clone());

                self.next();

                if !matches!(self.current, Ok(Token::Comma)) {
                    break;
                } else {
                    self.next();
                }
            }

            expect!(
                self.current()?,
                Token::RightBrak,
                self.lexer.span(),
                "expected ']' after use",
            );

            self.next();

            import_symbols = Some(imports);
        }

        expect!(
            self.current()?,
            Token::SemiColon,
            self.lexer.span(),
            "expected ';'"
        );

        self.next();

        Ok(TopLevelExpr::Use(Box::new(Use {
            module: mod_name,
            imports: import_symbols,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_export(&mut self) -> ParseResult<TopLevelExpr> {
        todo!()
    }

    fn parse_standalone_expression(&mut self) -> ParseResult<Expr> {
        let expr = self.parse_expression()?;

        expect!(
            self.current()?,
            Token::SemiColon,
            self.lexer.span(),
            "expected ';' after an expression"
        );

        self.next();

        Ok(expr)
    }

    fn parse_expression(&mut self) -> ParseResult<Expr> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> ParseResult<Expr> {
        let span = self.lexer.span();
        let expr = self.parse_or()?;

        match self.current()? {
            Token::Eq => self.parse_assign_var(span, expr),
            Token::PlusEq => {
                self.parse_binary_assign(span, expr, BinaryOpKind::Add)
            }
            Token::MinusEq => {
                self.parse_binary_assign(span, expr, BinaryOpKind::Sub)
            }
            Token::MulEq => {
                self.parse_binary_assign(span, expr, BinaryOpKind::Mul)
            }
            Token::DivEq => {
                self.parse_binary_assign(span, expr, BinaryOpKind::Div)
            }
            Token::RemEq => {
                self.parse_binary_assign(span, expr, BinaryOpKind::Rem)
            }
            _ => Ok(expr),
        }
    }

    fn parse_assign_var(
        &mut self,
        span: Span,
        left: Expr,
    ) -> ParseResult<Expr> {
        expect!(self.current()?, Token::Eq, self.lexer.span(), "expected '='");

        self.next();

        let right_val = self.parse_expression()?;

        Ok(Expr::AssignVar(Box::new(AssignVar {
            left,
            value: right_val,
            type_: Type::Unknown,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_binary_assign(
        &mut self,
        span: Span,
        expr: Expr,
        bin_op_kind: BinaryOpKind,
    ) -> ParseResult<Expr> {
        let op_span = self.lexer.span();

        self.next();

        let bin_op = BinaryOp { kind: bin_op_kind, span: op_span };

        let bin =
            Expr::binary(expr.clone(), bin_op, self.parse_expression()?, None);

        Ok(Expr::AssignVar(Box::new(AssignVar {
            left: expr,
            value: bin,
            type_: Type::Unknown,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_or(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_and()?;

        while matches!(self.current, Ok(Token::Or)) {
            let span = self.lexer.span();

            self.next();

            expr = Expr::binary(
                expr,
                BinaryOp { kind: BinaryOpKind::Or, span },
                self.parse_and()?,
                Some(Type::Bool),
            )
        }

        Ok(expr)
    }

    fn parse_and(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_eq()?;

        while matches!(self.current, Ok(Token::And)) {
            let span = self.lexer.span();

            self.next();

            expr = Expr::binary(
                expr,
                BinaryOp { kind: BinaryOpKind::And, span },
                self.parse_eq()?,
                Some(Type::Bool),
            )
        }

        Ok(expr)
    }

    fn parse_eq(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_compare()?;

        while matches!(self.current, Ok(Token::NotEq) | Ok(Token::DEq)) {
            let span = self.lexer.span();
            let bin_op_kind = BinaryOpKind::from(self.current()?);
            let bin_op = BinaryOp { kind: bin_op_kind, span };

            self.next();

            expr = Expr::binary(
                expr,
                bin_op,
                self.parse_compare()?,
                Some(Type::Bool),
            );
        }

        Ok(expr)
    }

    fn parse_compare(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_term()?;

        while matches!(
            self.current,
            Ok(Token::GT) | Ok(Token::LT) | Ok(Token::GE) | Ok(Token::LE)
        ) {
            let span = self.lexer.span();
            let bin_op_kind = BinaryOpKind::from(self.current()?);
            let bin_op = BinaryOp { kind: bin_op_kind, span };

            self.next();

            expr = Expr::binary(
                expr,
                bin_op,
                self.parse_term()?,
                Some(Type::Bool),
            );
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_factor()?;

        while matches!(self.current, Ok(Token::Minus) | Ok(Token::Plus)) {
            let span = self.lexer.span();
            let bin_op_kind = BinaryOpKind::from(self.current()?);
            let bin_op = BinaryOp { kind: bin_op_kind, span };

            self.next();

            expr = Expr::binary(expr, bin_op, self.parse_factor()?, None);
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_unary()?;

        while matches!(
            self.current,
            Ok(Token::Div) | Ok(Token::Rem) | Ok(Token::Mul)
        ) {
            let span = self.lexer.span();
            let bin_op_kind = BinaryOpKind::from(self.current()?);
            let bin_op = BinaryOp { kind: bin_op_kind, span };

            self.next();

            expr = Expr::binary(expr, bin_op, self.parse_unary()?, None);
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> ParseResult<Expr> {
        let expr = if matches!(self.current, Ok(Token::Not) | Ok(Token::Minus))
        {
            let span = self.lexer.span();
            let unary_op_kind = UnaryOpKind::from(self.current()?);
            let unary_op = UnaryOp { kind: unary_op_kind, span };
            let type_ = if let UnaryOpKind::NegBool = unary_op_kind {
                Some(Type::Bool)
            } else {
                None
            };

            self.next();

            Expr::unary(unary_op, self.parse_unary()?, type_)
        } else {
            self.parse_call(None)?
        };

        Ok(expr)
    }

    fn parse_call(&mut self, mut arg: Option<Expr>) -> ParseResult<Expr> {
        let mut expr = self.parse_primary()?;

        loop {
            match self.current()? {
                Token::LeftParen => {
                    expr = self.finish_call(expr, arg)?;
                    arg = None;
                }

                Token::Dot => {
                    self.next();
                    expr = self.parse_call(Some(expr))?;
                    break;
                }

                Token::LeftBrak => {
                    if arg.is_some() {
                        throw_error!(
                            self.lexer.span(),
                            "unexpected expressions: {:?}",
                            arg.unwrap()
                        );
                    }

                    expr = self.parse_indexing(expr)?;
                }

                Token::LeftBrace => {
                    if arg.is_some() {
                        throw_error!(
                            self.lexer.span(),
                            "unexpected expressions: {:?}",
                            arg.unwrap()
                        );
                    }

                    expr = self.parse_struct_init(expr)?;
                }

                Token::Colon => {
                    if arg.is_some() {
                        throw_error!(
                            self.lexer.span(),
                            "unexpected expressions: {:?}",
                            arg.unwrap()
                        );
                    }

                    expr = self.parse_enum_var_access(expr)?;
                }

                Token::SingleQuote => {
                    if arg.is_some() {
                        throw_error!(
                            self.lexer.span(),
                            "unexpected expressions: {:?}",
                            arg.unwrap()
                        );
                    }

                    expr = self.parse_struct_field_access(expr)?;
                }

                Token::DColon => {
                    if arg.is_some() {
                        throw_error!(
                            self.lexer.span(),
                            "unexpected expressions: {:?}",
                            arg.unwrap()
                        );
                    }

                    expr = self.parse_module_access(expr)?;
                }

                _ => break,
            }
        }

        Ok(expr)
    }

    fn parse_module_access(&mut self, left: Expr) -> ParseResult<Expr> {
        let span = self.lexer.span();

        let module: String;

        match left {
            Expr::Ident(ident) => module = ident.name,
            _ => {
                throw_error!(left.get_span().clone(), "expected a module name")
            }
        }

        expect!(
            self.current()?,
            Token::DColon,
            self.lexer.span(),
            "expected '::'"
        );

        self.next();

        let Token::Ident(constant) = expect!(
            self.current()?,
            Token::Ident(..),
            self.lexer.span(),
            "expected an identifier"
        ) else {
            unreachable!()
        };

        let constant = constant.clone();

        self.next();

        Ok(Expr::ModAccess(Box::new(ModAccess {
            module: module.clone(),
            constant,
            span: combine(&span, &self.lexer.span()),
            type_: Type::Unknown,
        })))
    }

    fn parse_struct_field_access(&mut self, left: Expr) -> ParseResult<Expr> {
        let span = self.lexer.span();

        let source: String;

        match left {
            Expr::Ident(ident) => source = ident.name,
            _ => {
                throw_error!(left.get_span().clone(), "expected a struct name")
            }
        }

        expect!(
            self.current()?,
            Token::SingleQuote,
            self.lexer.span(),
            "expected \"'\""
        );

        self.next();

        let Token::Ident(field) = expect!(
            self.current()?,
            Token::Ident(..),
            self.lexer.span(),
            "expected a field name"
        ) else {
            unreachable!()
        };

        let field = field.clone();

        self.next();

        Ok(Expr::StructFieldAccess(Box::new(StructFieldAccess {
            source: source.clone(),
            field,
            span: combine(&span, &self.lexer.span()),
            type_: Type::Unknown,
        })))
    }

    fn parse_enum_var_access(&mut self, left: Expr) -> ParseResult<Expr> {
        let span = self.lexer.span();

        let source: String;

        match left {
            Expr::Ident(ident) => source = ident.name,
            _ => {
                throw_error!(left.get_span().clone(), "expected an enum name")
            }
        }

        expect!(
            self.current()?,
            Token::Colon,
            self.lexer.span(),
            "expected ':'"
        );

        self.next();

        let Token::Ident(variant) = expect!(
            self.current()?,
            Token::Ident(..),
            self.lexer.span(),
            "expected a variant name"
        ) else {
            unreachable!()
        };

        let variant = variant.clone();

        self.next();

        Ok(Expr::EnumVarAccess(Box::new(EnumVarAccess {
            source: source.clone(),
            variant,
            span: combine(&span, &self.lexer.span()),
            type_: Type::Named(source),
        })))
    }

    fn parse_struct_init(&mut self, left: Expr) -> ParseResult<Expr> {
        let span = self.lexer.span();

        let struct_name: String;

        match left {
            Expr::Ident(ident) => struct_name = ident.name,
            _ => {
                throw_error!(left.get_span().clone(), "expected a struct name")
            }
        }

        expect!(
            self.current()?,
            Token::LeftBrace,
            self.lexer.span(),
            "expected '{'"
        );

        self.next();

        let mut init_args = Vec::<StructInitArg>::new();

        while !matches!(self.current, Ok(Token::RightBrace)) {
            init_args.push(self.parse_struct_init_arg()?);

            if !matches!(self.current, Ok(Token::Comma)) {
                break;
            } else {
                self.next();
            }
        }

        expect!(
            self.current()?,
            Token::RightBrace,
            self.lexer.span(),
            "expected '}'"
        );

        self.next();

        Ok(Expr::StructInit(Box::new(StructInit {
            name: struct_name.clone(),
            arguments: init_args,
            type_: Type::Named(struct_name),
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_struct_init_arg(&mut self) -> ParseResult<StructInitArg> {
        let span = self.lexer.span();

        let Token::Ident(name) = expect!(
            self.current()?,
            Token::Ident(..),
            self.lexer.span(),
            "expected a field name"
        ) else {
            unreachable!()
        };

        let name = name.clone();

        self.next();

        let value = self.parse_expression()?;

        Ok(StructInitArg {
            name,
            value,
            span: combine(&span, &self.lexer.span()),
        })
    }

    fn parse_indexing(&mut self, source: Expr) -> ParseResult<Expr> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::LeftBrak,
            self.lexer.span(),
            "expected '['"
        );

        self.next();

        let index = self.parse_expression()?;

        expect!(
            self.current()?,
            Token::RightBrak,
            self.lexer.span(),
            "expected ']"
        );

        self.next();

        Ok(Expr::Index(Box::new(Index {
            source,
            index,
            type_: Type::Unknown,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn finish_call(
        &mut self,
        callee: Expr,
        arg: Option<Expr>,
    ) -> ParseResult<Expr> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::LeftParen,
            self.lexer.span(),
            "expected '('"
        );

        self.next();

        let mut args = Vec::<Expr>::new();

        if let Some(arg) = arg {
            args.push(arg);
        }

        if !matches!(self.current, Ok(Token::RightParen)) {
            args.push(self.parse_expression()?);

            while matches!(self.current, Ok(Token::Comma)) {
                self.next();

                if args.len() > 127 {
                    // C99 standard
                    throw_error!(
                        combine(&span, &self.lexer.span(),),
                        "too many arguments passed to function"
                    );
                }

                args.push(self.parse_expression()?);
            }
        }

        expect!(
            self.current()?,
            Token::RightParen,
            self.lexer.span(),
            "expected ')' after function call"
        );

        self.next();

        // Check for `<.`
        if matches!(self.current, Ok(Token::LDot)) {
            self.next();
            args.push(self.parse_expression()?);
        }

        Ok(Expr::Call(Box::new(Call {
            callee,
            arguments: args,
            type_: Type::Unknown,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        let span = self.lexer.span();

        match self.current {
            Ok(Token::True) => {
                self.next();
                Ok(Expr::Bool(Box::new(BoolLiteral { value: true, span })))
            }

            Ok(Token::False) => {
                self.next();
                Ok(Expr::Bool(Box::new(BoolLiteral { value: false, span })))
            }

            Ok(Token::Int(value)) => {
                self.next();
                Ok(Expr::Int(Box::new(IntLiteral { value, span })))
            }

            Ok(Token::Float(value)) => {
                self.next();
                Ok(Expr::Float(Box::new(FloatLiteral { value, span })))
            }

            Ok(Token::Str(ref value)) => {
                let value = value.clone();
                self.next();
                Ok(Expr::Str(Box::new(StrLiteral { value, span })))
            }

            Ok(Token::LeftBrak) => self.parse_list_literal(),

            Ok(Token::Hash) => self.parse_tuple_literal(),

            Ok(Token::Ident(ref ident)) => {
                let ident = ident.clone();
                self.next();
                Ok(Expr::Ident(Box::new(Ident {
                    name: ident,
                    span,
                    type_: Type::Unknown,
                })))
            }

            Ok(Token::BackSlash) => self.parse_closure(),

            Ok(Token::LeftParen) => self.parse_group(),

            Ok(Token::If) => self.parse_if(),

            Ok(Token::When) => self.parse_when(),

            Ok(Token::While) => self.parse_while(),

            Ok(Token::Let) => self.parse_let(),

            Ok(Token::Break) => self.parse_break(),

            Ok(Token::Return) => self.parse_return(),

            Ok(Token::Void) => {
                self.next();
                Ok(Expr::Void(Box::new(Void { span })))
            }

            Ok(Token::Match) => self.parse_match(),

            Err(ref err) => {
                Err(Spanned(ParseErrorKind::LexErr(err.clone()), span))
            }

            _ => {
                let current = self.current()?.clone();
                self.next();
                throw_error!(span, "unexpected token: '{:?}'", current);
            }
        }
    }

    fn parse_match(&mut self) -> ParseResult<Expr> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::Match,
            self.lexer.span(),
            "expected 'match'"
        );

        self.next();

        let expr = self.parse_expression()?;

        expect!(
            self.current()?,
            Token::LeftBrace,
            self.lexer.span(),
            "expected '{'"
        );

        self.next();

        let mut cases = Vec::<MatchCase>::new();
        while !matches!(self.current, Ok(Token::RightBrace)) {
            cases.push(self.parse_match_case()?);

            if !matches!(self.current, Ok(Token::Comma)) {
                break;
            } else {
                self.next();
            }
        }

        expect!(
            self.current()?,
            Token::RightBrace,
            self.lexer.span(),
            "expected '}'"
        );

        self.next();

        Ok(Expr::Match(Box::new(Match {
            expression: expr,
            expressions: cases,
            type_: Type::Unknown,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_match_case(&mut self) -> ParseResult<MatchCase> {
        let span = self.lexer.span();

        let pattern = self.parse_pattern()?;

        let guard = if matches!(self.current, Ok(Token::If)) {
            self.next();
            Some(self.parse_expression()?)
        } else {
            None
        };

        let case_body = if matches!(self.current, Ok(Token::Then)) {
            self.next();
            vec![self.parse_expression()?]
        } else {
            self.parse_curly_body()?
        };

        Ok(MatchCase {
            pattern,
            guard,
            body: case_body,
            type_: Type::Unknown,
            span: combine(&span, &self.lexer.span()),
        })
    }

    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        let span = self.lexer.span();

        let mut pattern = match self.current()?.clone() {
            Token::Int(value) => {
                self.next();
                Pattern::Int(Box::new(IntLiteral { value, span }))
            }

            Token::Float(value) => {
                self.next();
                Pattern::Float(Box::new(FloatLiteral { value, span }))
            }

            Token::Str(value) => {
                self.next();
                Pattern::Str(Box::new(StrLiteral { value, span }))
            }

            Token::True => {
                self.next();
                Pattern::Bool(Box::new(BoolLiteral { value: true, span }))
            }

            Token::False => {
                self.next();
                Pattern::Bool(Box::new(BoolLiteral { value: false, span }))
            }

            Token::Underscore => {
                self.next();
                Pattern::Wildcard(span)
            }

            Token::LeftBrak => self.parse_list_pattern()?,

            Token::LT => self.parse_tuple_pattern()?,

            Token::Ident(ident) => self.parse_ident_pattern(ident)?,

            _ => {
                throw_error!(span, "invalid pattern: {:?}", self.current()?);
            }
        };

        let or_span = self.lexer.span();

        let mut or_patterns = Vec::<Pattern>::new();
        while matches!(self.current, Ok(Token::MatchOr)) {
            self.next();
            or_patterns.push(self.parse_pattern()?);
        }

        if or_patterns.len() > 0 {
            pattern = Pattern::Or(Box::new(OrPattern {
                patterns: or_patterns,
                span: combine(&or_span, &self.lexer.span()),
            }));
        }

        Ok(pattern)
    }

    fn parse_ident_pattern(&mut self, ident: String) -> ParseResult<Pattern> {
        let span = self.lexer.span();

        self.next();

        match self.current()? {
            Token::LeftBrace => self.parse_struct_pattern(span, ident),
            Token::Colon => self.parse_enum_var_pattern(span, ident),
            _ => Ok(Pattern::Ident(Box::new(Ident {
                name: ident,
                span,
                type_: Type::Unknown,
            }))),
        }
    }

    fn parse_struct_pattern(
        &mut self,
        span: Span,
        ident: String,
    ) -> ParseResult<Pattern> {
        expect!(self.current()?, Token::LeftBrace, span, "expected '{'");

        self.next();

        let mut field_patterns = Vec::<FieldPattern>::new();
        while !matches!(self.current, Ok(Token::RightBrace)) {
            field_patterns.push(self.parse_field_pattern()?);

            if !matches!(self.current, Ok(Token::Comma)) {
                break;
            } else {
                self.next();
            }
        }

        expect!(
            self.current()?,
            Token::RightBrace,
            self.lexer.span(),
            "expected '}'"
        );

        self.next();

        Ok(Pattern::Struct(Box::new(StructPattern {
            source: ident,
            fields: field_patterns,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_field_pattern(&mut self) -> ParseResult<FieldPattern> {
        let span = self.lexer.span();

        let Token::Ident(name) = expect!(
            self.current()?,
            Token::Ident(..),
            self.lexer.span(),
            "expected a field name"
        ) else {
            unreachable!()
        };

        let name = name.clone();

        self.next();

        let pattern = self.parse_pattern()?;

        Ok(FieldPattern {
            name,
            pattern,
            span: combine(&span, &self.lexer.span()),
        })
    }

    fn parse_enum_var_pattern(
        &mut self,
        span: Span,
        ident: String,
    ) -> ParseResult<Pattern> {
        expect!(self.current()?, Token::Colon, span, "expected ':'");

        self.next();

        let Token::Ident(variant) = expect!(
            self.current()?,
            Token::Ident(..),
            self.lexer.span(),
            "expected a variant name"
        ) else {
            unreachable!()
        };

        let variant = variant.clone();

        self.next();

        let enum_var_access = EnumVarAccess {
            source: ident,
            variant,
            span: combine(&span, &self.lexer.span()),
            type_: Type::Unknown,
        };

        if matches!(self.current, Ok(Token::LeftParen)) {
            self.next();

            let mut arguments = Vec::<Pattern>::new();
            while !matches!(self.current, Ok(Token::RightParen)) {
                arguments.push(self.parse_pattern()?);

                if !matches!(self.current, Ok(Token::Comma)) {
                    break;
                } else {
                    self.next();
                }
            }

            expect!(
                self.current()?,
                Token::RightParen,
                self.lexer.span(),
                "expected ')'"
            );

            self.next();

            Ok(Pattern::Variant(Box::new(EnumVarPattern::VarInit(Box::new(
                EnumVarInitPattern {
                    access: enum_var_access,
                    arguments,
                    span: combine(&span, &self.lexer.span()),
                },
            )))))
        } else {
            Ok(Pattern::Variant(Box::new(EnumVarPattern::VarAccess(Box::new(
                enum_var_access,
            )))))
        }
    }

    fn parse_tuple_pattern(&mut self) -> ParseResult<Pattern> {
        let span = self.lexer.span();

        expect!(self.current()?, Token::LT, self.lexer.span(), "expected '<'");

        self.next();

        let mut values = Vec::<Pattern>::new();
        while !matches!(self.current, Ok(Token::GT)) {
            values.push(self.parse_pattern()?);

            if !matches!(self.current, Ok(Token::Comma)) {
                break;
            } else {
                self.next();
            }
        }

        expect!(self.current()?, Token::GT, self.lexer.span(), "expected '>'");

        self.next();

        Ok(Pattern::Tuple(Box::new(TuplePattern {
            values,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_list_pattern(&mut self) -> ParseResult<Pattern> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::LeftBrak,
            self.lexer.span(),
            "expected '['"
        );

        self.next();

        let mut elements = Vec::<Pattern>::new();
        while !matches!(self.current, Ok(Token::RightBrak)) {
            elements.push(self.parse_pattern()?);

            if !matches!(self.current, Ok(Token::Comma)) {
                break;
            } else {
                self.next();
            }
        }

        expect!(
            self.current()?,
            Token::RightBrak,
            self.lexer.span(),
            "expected '}'"
        );

        self.next();

        Ok(Pattern::List(Box::new(ListPattern {
            elements,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_return(&mut self) -> ParseResult<Expr> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::Return,
            self.lexer.span(),
            "expected 'return'"
        );

        self.next();

        let return_value = self.parse_expression()?;

        Ok(Expr::Return(Box::new(Return {
            value: return_value,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_break(&mut self) -> ParseResult<Expr> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::Break,
            self.lexer.span(),
            "expected 'break'"
        );

        self.next();

        Ok(Expr::Break(Box::new(Break { span })))
    }

    fn parse_let(&mut self) -> ParseResult<Expr> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::Let,
            self.lexer.span(),
            "expected 'let'"
        );

        self.next();

        let ident_span = self.lexer.span();

        let Token::Ident(name) = expect!(
            self.current()?,
            Token::Ident(..),
            self.lexer.span(),
            "expected an identifier"
        ) else {
            unreachable!()
        };

        let name = name.clone();

        self.next();

        let type_ = if !matches!(self.current, Ok(Token::Eq)) {
            self.parse_type()?
        } else {
            Spanned(Type::Unknown, ident_span)
        };

        expect!(self.current()?, Token::Eq, self.lexer.span(), "expected '='");

        self.next();

        let value = self.parse_expression()?;

        Ok(Expr::DefVar(Box::new(DefVar {
            name,
            value,
            type_,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_while(&mut self) -> ParseResult<Expr> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::While,
            self.lexer.span(),
            "expected 'while'"
        );

        self.next();

        let cond = self.parse_expression()?;

        let body = self.parse_curly_body()?;

        Ok(Expr::While(Box::new(While {
            condition: cond,
            body,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_when(&mut self) -> ParseResult<Expr> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::When,
            self.lexer.span(),
            "expected 'when'"
        );

        self.next();

        let cond = self.parse_expression()?;

        let then_body = self.parse_curly_body()?;

        let else_body = if matches!(self.current, Ok(Token::Else)) {
            self.next();
            Some(self.parse_curly_body()?)
        } else {
            None
        };

        Ok(Expr::When(Box::new(When {
            condition: cond,
            then: then_body,
            else_: else_body,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_if(&mut self) -> ParseResult<Expr> {
        let span = self.lexer.span();

        expect!(self.current()?, Token::If, self.lexer.span(), "expected 'if'");

        self.next();

        let cond = self.parse_expression()?;

        let then_body = if matches!(self.current, Ok(Token::Then)) {
            self.next();
            vec![self.parse_standalone_expression()?]
        } else {
            self.parse_curly_body()?
        };

        expect!(
            self.current()?,
            Token::Else,
            self.lexer.span(),
            "expected 'else'"
        );

        self.next();

        let else_body = if matches!(self.current, Ok(Token::LeftBrace)) {
            self.parse_curly_body()?
        } else {
            vec![self.parse_standalone_expression()?]
        };

        Ok(Expr::If(Box::new(If {
            condition: cond,
            then: then_body,
            else_: else_body,
            type_: Type::Unknown,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_group(&mut self) -> ParseResult<Expr> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::LeftParen,
            self.lexer.span(),
            "expected '('"
        );

        self.next();

        let expr = self.parse_expression()?;
        let type_ = expr.get_type().clone();

        expect!(
            self.current()?,
            Token::RightParen,
            self.lexer.span(),
            "expected ')'"
        );

        self.next();

        Ok(Expr::Group(Box::new(Group {
            expression: expr,
            type_,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_closure(&mut self) -> ParseResult<Expr> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::BackSlash,
            self.lexer.span(),
            "expected '\\'"
        );

        self.next();

        let params = self.parse_params()?;

        let return_type = self.parse_type()?;

        let body = if matches!(self.current, Ok(Token::Do)) {
            self.next();
            vec![self.parse_expression()?]
        } else {
            self.parse_curly_body()?
        };

        Ok(Expr::Closure(Box::new(Closure {
            parameters: params,
            return_type,
            body,
            type_: Type::Unknown,
            span: combine(&span, &self.lexer.span()),
        })))
    }

    fn parse_list_literal(&mut self) -> ParseResult<Expr> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::LeftBrak,
            self.lexer.span(),
            "expected '['"
        );

        self.next();

        let mut elements = Vec::<Expr>::new();
        while !matches!(self.current, Ok(Token::RightBrak)) {
            elements.push(self.parse_expression()?);

            if !matches!(self.current, Ok(Token::Comma)) {
                break;
            } else {
                self.next();
            }
        }

        expect!(
            self.current()?,
            Token::RightBrak,
            self.lexer.span(),
            "expected ']'"
        );

        self.next();

        Ok(Expr::List(Box::new(List {
            elements,
            span: combine(&span, &self.lexer.span()),
            type_: Type::List(Box::new(Type::Unknown)),
        })))
    }

    fn parse_tuple_literal(&mut self) -> ParseResult<Expr> {
        let span = self.lexer.span();

        expect!(
            self.current()?,
            Token::Hash,
            self.lexer.span(),
            "expected '#'"
        );

        self.next();

        expect!(
            self.current()?,
            Token::LeftParen,
            self.lexer.span(),
            "expected '('"
        );

        self.next();

        let mut values = Vec::<Expr>::new();
        while !matches!(self.current, Ok(Token::RightParen)) {
            values.push(self.parse_expression()?);

            if !matches!(self.current, Ok(Token::Comma)) {
                break;
            } else {
                self.next();
            }
        }

        expect!(
            self.current()?,
            Token::RightParen,
            self.lexer.span(),
            "expected ')'"
        );

        self.next();

        Ok(Expr::Tuple(Box::new(Tuple {
            values,
            span: combine(&span, &self.lexer.span()),
            type_: Type::Tup(vec![Type::Unknown]),
        })))
    }
}

#[cfg(test)]
mod tests {
    use similar_asserts::assert_eq;

    use super::*;

    #[track_caller]
    fn one_top(result: ModParseResult<Vec<TopLevelExpr>>) -> TopLevelExpr {
        if let Ok(mut top_level_exprs) = result {
            if top_level_exprs.len() == 1 {
                top_level_exprs.pop().unwrap()
            } else {
                panic!(
                    "parse contains more than one top level expression: {:?}",
                    &top_level_exprs
                );
            }
        } else {
            panic!("parse not successful: {:?}", result);
        }
    }

    #[track_caller]
    fn exprs(result: ModParseResult<Vec<TopLevelExpr>>) -> Vec<Expr> {
        if let Ok(mut top_level_exprs) = result {
            if top_level_exprs.len() == 1 {
                if let TopLevelExpr::Func(func) = top_level_exprs.pop().unwrap()
                {
                    return func.closure.body;
                } else {
                    panic!("expected a function");
                }
            } else {
                panic!(
                    "parse contains more than one top level expression: {:?}",
                    &top_level_exprs
                );
            }
        } else {
            panic!("parse not successful: {:?}", result);
        }
    }

    #[track_caller]
    fn one_error(result: ModParseResult<Vec<TopLevelExpr>>) -> ParseError {
        if let Err(mut errs) = result {
            if errs.len() == 1 {
                errs.pop().unwrap()
            } else {
                panic!(
                    "parse contains more than one parse errors: {:?}",
                    &errs
                );
            }
        } else {
            panic!("parse successful: {:?}", result);
        }
    }

    #[track_caller]
    fn multi_errors(
        result: ModParseResult<Vec<TopLevelExpr>>,
        num_errs: usize,
    ) -> Vec<ParseError> {
        if let Err(errs) = result {
            if errs.len() == num_errs {
                errs
            } else {
                panic!(
                    "parse does not contain {} parse errors: {:?}",
                    num_errs, &errs
                );
            }
        } else {
            panic!("parse successfull: {:?}", result);
        }
    }

    macro_rules! parse_error {
        ($msg: expr, $range: expr) => {
            Spanned(ParseErrorKind::ParseErr($msg.to_string()), $range)
        };
    }
    macro_rules! unexpected_end {
        ($range: expr) => {
            Spanned(
                ParseErrorKind::LexErr(LexError::WithMessage(
                    "unexpected end of source".to_string(),
                )),
                $range,
            )
        };
    }

    #[test]
    fn empty() {
        assert_eq!(test_parse(""), Ok(vec![]));
        assert_eq!(test_parse("     "), Ok(vec![]));
        assert_eq!(test_parse("     \n"), Ok(vec![]));
    }

    #[test]
    fn use_exprs() {
        assert_eq!(
            result: one_top(test_parse("use fmt;")),
            expected: TopLevelExpr::Use(Box::new(Use {
                module: "fmt".to_string(),
                imports: None,
                span: 0..8
            }))
        );

        assert_eq!(
            result: one_error(test_parse("use")),
            expected: unexpected_end!(3..3)
        );

        assert_eq!(
            result: one_error(test_parse("use fmt")),
            expected: unexpected_end!(7..7)
        );

        assert_eq!(
            result: one_top(test_parse("use fmt [println, print];")),
            expected: TopLevelExpr::Use(Box::new(Use {
                module: "fmt".to_string(),
                imports: Some(vec!["println".to_string(), "print".to_string()]),
                span: 0..25
            })),
        );

        assert_eq!(
            result: one_top(test_parse("use fmt [];")),
            expected: TopLevelExpr::Use(Box::new(Use {
                module: "fmt".to_string(),
                imports: Some(vec![]),
                span: 0..11
            }))
        );

        assert_eq!(
            result: one_error(test_parse("use fmt [")),
            expected: unexpected_end!(9..9)
        );

        assert_eq!(
            result: one_error(test_parse("use fmt [println")),
            expected: unexpected_end!(16..16)
        );

        assert_eq!(
            result: multi_errors(test_parse("use fmt [;"), 2),
            expected: vec![
                parse_error!("expected an identifier", 9..10),
                parse_error!("expected a top-level expression but found 'SemiColon'", 9..10)
            ]
        );
    }

    #[test]
    #[ignore = "export expression is not implemented yet"]
    fn export_exprs() {}

    #[test]
    fn enum_without_generics() {
        assert_eq!(
            result: one_top(test_parse("enum int_option { Some(int), None }")),
            expected: TopLevelExpr::EnumDef(Box::new(EnumDef {
                name: "int_option".to_string(),
                variants: vec![
                    EnumVarDef {
                        name: "Some".to_string(),
                        types: vec![Spanned(Type::Int, 23..26)],
                        span: 18..28
                    },
                    EnumVarDef {
                        name: "None".to_string(),
                        types: vec![],
                        span: 29..35
                    }
                ],
                span: 0..35
            }))
        );

        assert_eq!(
            result: multi_errors(test_parse("enum int_option { Some(int) None }"), 3),
            expected: vec![
                parse_error!("expected '}' after enum variants", 28..32),
                parse_error!("expected a top-level expression but found 'Ident(\"None\")'", 28..32),
                parse_error!("expected a top-level expression but found 'RightBrace'", 33..34),
            ]
        );

        assert_eq!(
            result: multi_errors(test_parse("enum;"), 2),
            expected: vec![
                parse_error!("expected an enum name", 4..5),
                parse_error!("expected a top-level expression but found 'SemiColon'", 4..5)
            ],
        );
    }

    #[test]
    #[ignore = "generics is not implemented yet"]
    fn enum_with_generics() {}

    #[test]
    fn struct_without_generics() {
        assert_eq!(
            result: one_top(test_parse("struct Person {}")),
            expected: TopLevelExpr::StructDef(Box::new(StructDef {
                name: "Person".to_string(),
                fields: vec![],
                span: 0..16
            }))
        );

        assert_eq!(
            result: one_top(test_parse("struct Person { name str, age int }")),
            expected: TopLevelExpr::StructDef(Box::new(StructDef {
                name: "Person".to_string(),
                fields: vec![
                    StructFieldDef {
                        name: "name".to_string(),
                        type_: Spanned(Type::Str, 21..24),
                        span: 16..25
                    },
                    StructFieldDef {
                        name: "age".to_string(),
                        type_: Spanned(Type::Int, 30..33),
                        span: 26..35
                    }
                ],
                span: 0..35
            }))
        );

        assert_eq!(
            result: multi_errors(test_parse("struct Person { name int;"), 2),
            expected: vec![
                parse_error!("expected '}' after struct fields", 24..25),
                parse_error!("expected a top-level expression but found 'SemiColon'", 24..25)
            ],
        );

        assert_eq!(
            result: one_error(test_parse("struct Person")),
            expected: unexpected_end!(13..13),
        );
    }

    #[test]
    #[ignore = "generics is not implemented yet"]
    fn struct_with_generics() {}

    #[test]
    fn func_without_generics() {
        assert_eq!(
            result: one_top(test_parse("func main(args []str) void {}")),
            expected: TopLevelExpr::Func(Box::new(Func {
                pure: false,
                rec: false,
                name: "main".to_string(),
                closure: Closure {
                    parameters: vec![
                        FuncParam {
                            name: "args".to_string(),
                            type_: Spanned(Type::List(Box::new(Type::Str)), 15..20),
                            span: 10..21
                        }
                    ],
                    return_type: Spanned(Type::Void, 22..26),
                    body: vec![],
                    type_: Type::Unknown,
                    span: 0..29,
                },
                span: 0..29
            })),
        );

        assert_eq!(
            result: one_top(test_parse("func main() void do void;")),
            expected: TopLevelExpr::Func(Box::new(Func {
                pure: false,
                rec: false,
                name: "main".to_string(),
                closure: Closure {
                    parameters: vec![],
                    return_type: Spanned(Type::Void, 12..16),
                    body: vec![
                        Expr::Void(Box::new(Void { span: 20..24 }))
                    ],
                    type_: Type::Unknown,
                    span: 0..25
                },
                span: 0..25
            }))
        );

        assert_eq!(
            result: one_error(test_parse("func main()")),
            expected: unexpected_end!(11..11)
        );

        assert_eq!(
            result: multi_errors(test_parse("func main() void }"), 2),
            expected: vec![
                parse_error!("expected '{' for the start of a function body", 17..18),
                parse_error!("expected a top-level expression but found 'RightBrace'", 17..18)
            ]
        );

        assert_eq!(
            result: multi_errors(test_parse("func main void"), 2),
            expected: vec![
                parse_error!("expected '('", 10..14),
                parse_error!("expected a top-level expression but found 'Void'", 10..14)
            ]
        );
    }

    #[test]
    #[ignore = "generics is not implemented yet"]
    fn func_with_generics() {}

    #[test]
    fn literals() {
        assert_eq!(
            result: exprs(test_parse(
                "func main() void { 123; 1.23; \"Bello\"; true; [1, 2, 3]; #(\"bello\", 2.2, 3); some_var; }"
            )),
            expected: vec![
                Expr::Int(Box::new(IntLiteral {
                    value: 123,
                    span: 19..22
                })),
                Expr::Float(Box::new(FloatLiteral {
                    value: 1.23,
                    span: 24..28
                })),
                Expr::Str(Box::new(StrLiteral {
                    value: "Bello".to_string(),
                    span: 30..37
                })),
                Expr::Bool(Box::new(BoolLiteral {
                    value: true,
                    span: 39..43
                })),
                Expr::List(Box::new(List {
                    elements: vec![
                        Expr::Int(Box::new(IntLiteral {
                            value: 1,
                            span: 46..47
                        })),
                        Expr::Int(Box::new(IntLiteral {
                            value: 2,
                            span: 49..50
                        })),
                        Expr::Int(Box::new(IntLiteral {
                            value: 3,
                            span: 52..53
                        })),
                    ],
                    span: 45..55,
                    type_: Type::List(Box::new(Type::Unknown)),
                })),
                Expr::Tuple(Box::new(Tuple {
                    values: vec![
                        Expr::Str(Box::new(StrLiteral {
                            value: "bello".to_string(),
                            span: 58..65
                        })),
                        Expr::Float(Box::new(FloatLiteral {
                            value: 2.2,
                            span: 67..70
                        })),
                        Expr::Int(Box::new(IntLiteral {
                            value: 3,
                            span: 72..73
                        })),
                    ],
                    span: 56..75,
                    type_: Type::Tup(vec![Type::Unknown]),
                })),
                Expr::Ident(Box::new(Ident {
                    name: "some_var".to_string(),
                    span: 76..84,
                    type_: Type::Unknown
                }))
            ]
        );
    }

    #[test]
    fn enum_struct_init() {
        assert_eq!(
            result: exprs(test_parse(
                "func main() void { list:Cons(123) <. list:Cons(321) <. list:Nil; Person { age 18, name \"Nobu\", other stuff }; }"
            )),
            expected: vec![Expr::Call(Box::new(Call {
                    callee: Expr::EnumVarAccess(Box::new(EnumVarAccess {
                        source: "list".to_string(),
                        variant: "Cons".to_string(),
                        span: 23..29,
                        type_: Type::Named("list".to_string())
                    })),
                    arguments: vec![
                        Expr::Int(Box::new(IntLiteral {
                            value: 123,
                            span: 29..32
                        })),
                        Expr::Call(Box::new(Call {
                            callee: Expr::EnumVarAccess(Box::new(EnumVarAccess {
                                source: "list".to_string(),
                                variant: "Cons".to_string(),
                                span: 41..47,
                                type_: Type::Named("list".to_string())
                            })),
                            arguments: vec![
                                Expr::Int(Box::new(IntLiteral {
                                    value: 321,
                                    span: 47..50
                                })),
                                Expr::EnumVarAccess(Box::new(EnumVarAccess {
                                    source: "list".to_string(),
                                    variant: "Nil".to_string(),
                                    span: 59..64,
                                    type_: Type::Named("list".to_string())
                                }))
                            ],
                            type_: Type::Unknown,
                            span: 46..64
                        }))
                    ],
                    type_: Type::Unknown,
                    span: 28..64
                })),
                Expr::StructInit(Box::new(StructInit {
                    name: "Person".to_string(),
                    arguments: vec![
                        StructInitArg {
                            name: "age".to_string(),
                            value: Expr::Int(Box::new(IntLiteral {
                                value: 18,
                                span: 78..80
                            })),
                            span: 74..81
                        },
                        StructInitArg {
                            name: "name".to_string(),
                            value: Expr::Str(Box::new(StrLiteral {
                                value: "Nobu".to_string(),
                                span: 87..93
                            })),
                            span: 82..94
                        },
                        StructInitArg {
                            name: "other".to_string(),
                            value: Expr::Ident(Box::new(Ident {
                                name: "stuff".to_string(),
                                span: 101..106,
                                type_: Type::Unknown
                            })),
                            span: 95..108
                        }
                    ],
                    type_: Type::Named("Person".to_string()),
                    span: 72..109
                }))
            ]
        );
    }

    #[test]
    fn struct_field_access() {
        assert_eq!(
            result: exprs(test_parse("func main() void { person'name.length(); }")),
            expected: vec![
                Expr::Call(Box::new(Call {
                    callee: Expr::Ident(Box::new(Ident {
                        name: "length".to_string(),
                        span: 31..37,
                        type_: Type::Unknown
                    })),
                    arguments: vec![
                        Expr::StructFieldAccess(Box::new(StructFieldAccess {
                            source: "person".to_string(),
                            field: "name".to_string(),
                            span: 25..31,
                            type_: Type::Unknown
                        }))
                    ],
                    span: 37..40,
                    type_: Type::Unknown
                }))
            ]
        );
    }

    #[test]
    fn mod_access() {
        assert_eq!(
            result: exprs(test_parse("func main() void { fmt::println(\"Hello World\"); }")),
            expected: vec![
                Expr::Call(Box::new(Call {
                    callee: Expr::ModAccess(Box::new(ModAccess {
                        module: "fmt".to_string(),
                        constant: "println".to_string(),
                        span: 22..32,
                        type_: Type::Unknown
                    })),
                    arguments: vec![
                        Expr::Str(Box::new(StrLiteral {
                            value: "Hello World".to_string(),
                            span: 32..45
                        }))
                    ],
                    type_: Type::Unknown,
                    span: 31..47
                }))
            ]
        );
    }

    #[test]
    fn closure() {
        assert_eq!(
            result: exprs(test_parse("func main() void { \\(name str) str do name; }")),
            expected: vec![
                Expr::Closure(Box::new(Closure {
                    parameters: vec![
                        FuncParam {
                            name: "name".to_string(),
                            type_: Spanned(Type::Str, 26..29),
                            span: 21..30
                        }
                    ],
                    return_type: Spanned(Type::Str, 31..34),
                    body: vec![
                        Expr::Ident(Box::new(Ident {
                            name: "name".to_string(),
                            span: 38..42,
                            type_: Type::Unknown
                        }))
                    ],
                    type_: Type::Unknown,
                    span: 19..43
                }))
            ]
        );

        assert_eq!(
            result: exprs(test_parse("func main() void { \\(name str) str { name; }; }")),
            expected: vec![
                Expr::Closure(Box::new(Closure {
                    parameters: vec![
                        FuncParam {
                            name: "name".to_string(),
                            type_: Spanned(Type::Str, 26..29),
                            span: 21..30
                        }
                    ],
                    return_type: Spanned(Type::Str, 31..34),
                    body: vec![
                        Expr::Ident(Box::new(Ident {
                            name: "name".to_string(),
                            span: 37..41,
                            type_: Type::Unknown
                        }))
                    ],
                    type_: Type::Unknown,
                    span: 19..45
                }))
            ]
        );
    }

    #[test]
    fn binary_unary() {
        assert_eq!(
            result: exprs(test_parse("func main() void { 1 + (1 - 1) / 1; }")),
            expected: vec![
                Expr::Binary(Box::new(Binary {
                    lhs: Expr::Int(Box::new(IntLiteral {
                        value: 1,
                        span: 19..20
                    })),
                    rhs: Expr::Binary(Box::new(Binary {
                        lhs: Expr::Group(Box::new(Group {
                            expression: Expr::Binary(Box::new(Binary {
                                lhs: Expr::Int(Box::new(IntLiteral {
                                    value: 1,
                                    span: 24..25
                                })),
                                rhs: Expr::Int(Box::new(IntLiteral {
                                    value: 1,
                                    span: 28..29
                                })),
                                operator: BinaryOp {
                                    kind: BinaryOpKind::Sub,
                                    span: 26..27
                                },
                                span: 24..29,
                                type_: Type::Unknown
                            })),
                            type_: Type::Unknown,
                            span: 23..32
                        })),
                        rhs: Expr::Int(Box::new(IntLiteral {
                            value: 1,
                            span: 33..34
                        })),
                        operator: BinaryOp {
                            kind: BinaryOpKind::Div,
                            span: 31..32
                        },
                        span: 23..34,
                        type_: Type::Unknown
                    })),
                    operator: BinaryOp {
                        kind: BinaryOpKind::Add,
                        span: 21..22
                    },
                    span: 19..34,
                    type_: Type::Unknown
                }))
            ],
        );

        assert_eq!(
            result: exprs(test_parse("func main() void { not false; -(-23); }")),
            expected: vec![
                Expr::Unary(Box::new(Unary {
                    rhs: Expr::Bool(Box::new(BoolLiteral {
                        value: false,
                        span: 23..28
                    })),
                    operator: UnaryOp {
                        kind: UnaryOpKind::NegBool, span:
                        19..22
                    },
                    span: 19..28,
                    type_: Type::Bool
                })),
                Expr::Unary(Box::new(Unary {
                    rhs: Expr::Group(Box::new(Group {
                        expression: Expr::Int(Box::new(IntLiteral {
                            value: -23,
                            span: 32..35
                        })),
                        type_: Type::Int,
                        span: 31..37,
                    })),
                    operator: UnaryOp {
                        kind: UnaryOpKind::NegNum,
                        span: 30..31
                    },
                    span: 30..37,
                    type_: Type::Unknown
                }))
            ]
        );
    }

    #[test]
    fn variables() {
        assert_eq!(
            result: exprs(test_parse("func main() void { let a int = 0; a = 123; let b = a; }")),
            expected: vec![
                Expr::DefVar(Box::new(DefVar {
                    name: "a".to_string(),
                    value: Expr::Int(Box::new(IntLiteral {
                        value: 0,
                        span: 31..32
                    })),
                    span: 19..33,
                    type_: Spanned(Type::Int, 25..28)
                })),
                Expr::AssignVar(Box::new(AssignVar {
                    left: Expr::Ident(Box::new(Ident {
                        name: "a".to_string(),
                        span: 34..35,
                        type_: Type::Unknown
                    })),
                    value: Expr::Int(Box::new(IntLiteral {
                        value: 123,
                        span: 38..41
                    })),
                    span: 34..42,
                    type_: Type::Unknown
                })),
                Expr::DefVar(Box::new(DefVar {
                    name: "b".to_string(),
                    value: Expr::Ident(Box::new(Ident {
                        name: "a".to_string(),
                        span: 51..52,
                        type_: Type::Unknown
                    })),
                    span: 43..53,
                    type_: Spanned(Type::Unknown, 47..48)
                }))
            ]
        );
    }

    #[test]
    fn indexing() {
        assert_eq!(
            result: exprs(test_parse("func main() void { list[0]; #(1, 2)[idx + 1]; }")),
            expected: vec![
                Expr::Index(Box::new(Index {
                    source: Expr::Ident(Box::new(Ident {
                        name: "list".to_string(),
                        span: 19..23,
                        type_: Type::Unknown
                    })),
                    index: Expr::Int(Box::new(IntLiteral {
                        value: 0, span: 24..25
                    })),
                    span: 23..27,
                    type_: Type::Unknown
                })),
                Expr::Index(Box::new(Index {
                    source: Expr::Tuple(Box::new(Tuple {
                        values: vec![
                            Expr::Int(Box::new(IntLiteral {
                                value: 1, span: 30..31
                            })),
                            Expr::Int(Box::new(IntLiteral {
                                value: 2, span: 33..34
                            }))
                        ],
                        span: 28..36,
                        type_: Type::Tup(vec![Type::Unknown])
                    })),
                    index: Expr::Binary(Box::new(Binary {
                        lhs: Expr::Ident(Box::new(Ident {
                            name: "idx".to_string(),
                            span: 36..39, type_: Type::Unknown
                        })),
                        rhs: Expr::Int(Box::new(IntLiteral {
                            value: 1,
                            span: 42..43
                        })),
                        operator: BinaryOp {
                            kind: BinaryOpKind::Add,
                            span: 40..41
                        },
                        span: 36..43,
                        type_: Type::Unknown,
                    })),
                    span: 35..45,
                    type_: Type::Unknown
                }))
            ]
        );
    }
}
