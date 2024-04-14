use std::fs;

use logos::{Lexer, Logos};

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
    source: SourcePath,
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

    let (source, exprs) = Parser::new(source, lexer, current.unwrap()).parse();

    Ok(Module {
        name: module_name.clone(),
        path: source,
        expressions: exprs?,
        type_: Type::Named(module_name),
    })
}

impl<'a> Parser<'a> {
    fn new(
        source: SourcePath,
        lexer: Lexer<'a, Token>,
        current: Result<Token, LexError>,
    ) -> Self {
        Self {
            source,
            lexer,
            current,
            errors: Vec::new(),
            ast: Vec::new(),
            done: false,
        }
    }

    fn parse(mut self) -> (SourcePath, ModParseResult<Vec<TopLevelExpr>>) {
        while !self.done {
            let result = self.parse_top_level_expression();
            if let Err(err) = result {
                self.errors.push(err);
            } else {
                self.ast.push(result.unwrap());
            }
        }

        if self.errors.is_empty() {
            (self.source, Ok(self.ast))
        } else {
            (self.source, Err(self.errors))
        }
    }

    #[inline]
    fn next(&mut self) {
        if let Some(token) = self.lexer.next() {
            self.current = token;
        } else {
            self.done = true;
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

        match current {
            Token::Enum => self.parse_enum_def(),
            Token::Struct => self.parse_struct_def(),
            Token::Func | Token::Pure | Token::Rec => self.parse_func(),
            Token::Use => self.parse_use(),
            Token::Export => self.parse_export(),
            _ => throw_error!(
                self.lexer.span(),
                "expected a top-level expression but found '{:?}'",
                current
            ),
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
            "expected an identifier for an enum name"
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
                "expected ']' after import"
            );

            self.next();

            import_symbols = Some(imports);
        }

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

            self.next();

            Expr::unary(unary_op, self.parse_unary()?, None)
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
            Token::SingleQuote,
            self.lexer.span(),
            "expected \"'\""
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

        Ok(Expr::ModAccess(Box::new(ModAccess {
            module: module.clone(),
            constant,
            span: combine(&span, &self.lexer.span()),
            type_: Type::Named(module),
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

        Ok(Expr::StructFieldAccess(Box::new(StructFieldAccess {
            source: source.clone(),
            field,
            span: combine(&span, &self.lexer.span()),
            type_: Type::Named(source),
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
                Ok(Expr::Bool(Box::new(BoolLiteral { value: true, span })))
            }

            Ok(Token::False) => {
                Ok(Expr::Bool(Box::new(BoolLiteral { value: false, span })))
            }

            Ok(Token::Int(value)) => {
                Ok(Expr::Int(Box::new(IntLiteral { value, span })))
            }

            Ok(Token::Float(value)) => {
                Ok(Expr::Float(Box::new(FloatLiteral { value, span })))
            }

            Ok(Token::Str(ref value)) => Ok(Expr::Str(Box::new(StrLiteral {
                value: value.clone(),
                span,
            }))),

            Ok(Token::LeftBrak) => self.parse_list_literal(),

            Ok(Token::LT) => self.parse_tuple_literal(),

            Ok(Token::Ident(ref ident)) => Ok(Expr::Ident(Box::new(Ident {
                name: ident.clone(),
                span,
                type_: Type::Unknown,
            }))),

            Ok(Token::BackSlash) => self.parse_closure(),

            Err(ref err) => {
                Err(Spanned(ParseErrorKind::LexErr(err.clone()), span))
            }

            Ok(Token::If) => self.parse_if(),

            Ok(Token::When) => self.parse_when(),

            Ok(Token::While) => self.parse_while(),

            Ok(Token::Let) => self.parse_let(),

            Ok(Token::Break) => self.parse_break(),

            Ok(Token::Return) => self.parse_return(),

            Ok(Token::Void) => {
                Ok(Expr::Void(Box::new(Void { span: self.lexer.span() })))
            }

            Ok(Token::Match) => self.parse_match(),

            _ => {
                let current = self.current()?.clone();
                self.next();
                throw_error!(span, "unexpected token: '{:?}'", current);
            }
        }
    }

    fn parse_match(&mut self) -> ParseResult<Expr> {
        todo!()
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

        expect!(self.current()?, Token::LT, self.lexer.span(), "expected '<'");

        self.next();

        let mut values = Vec::<Expr>::new();
        while !matches!(self.current, Ok(Token::GT)) {
            values.push(self.parse_expression()?);

            if !matches!(self.current, Ok(Token::Comma)) {
                break;
            } else {
                self.next();
            }
        }

        expect!(self.current()?, Token::GT, self.lexer.span(), "expected '>'");

        self.next();

        Ok(Expr::Tuple(Box::new(Tuple {
            values,
            span: combine(&span, &self.lexer.span()),
            type_: Type::Tup(vec![Type::Unknown]),
        })))
    }
}
