use logos::{Lexer, Logos};
use std::fs;
use string_interner::symbol::SymbolU32;
use string_interner::DefaultStringInterner;

use crate::ast::*;
use crate::error::{combine, LexError, ParseError, ParseErrorKind, Span};
use crate::error::{SourcePath, Spanned};
use crate::lexer::Token;
use crate::types::Type;

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
    interner: &'a mut DefaultStringInterner,
    done: bool,
}

pub fn parse(
    source: SourcePath,
    interner: &mut DefaultStringInterner,
) -> ModParseResult<Module> {
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

    let module_name = interner.get_or_intern(module_name);

    if let Some(ref current) = current {
        if let Err(err) = current {
            return Err(vec![Spanned(
                ParseErrorKind::LexErr(err.clone()),
                lexer.span(),
            )]);
        }
    } else {
        return Ok(Module::new(module_name, source, 0..0));
    }

    let exprs = Parser::new(lexer, interner, current.unwrap()).parse();

    Ok(Module {
        name: module_name,
        path: source,
        expressions: exprs?,
        type_: Type::Named(0..0, module_name),
    })
}

#[cfg(test)]
pub fn test_parse(
    src: &str,
    interner: &mut DefaultStringInterner,
) -> ModParseResult<Vec<TopLevelExpr>> {
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

    Parser::new(lexer, interner, current.unwrap()).parse()
}

impl<'a> Parser<'a> {
    fn new(
        lexer: Lexer<'a, Token>,
        interner: &'a mut DefaultStringInterner,
        current: Result<Token, LexError>,
    ) -> Self {
        Self {
            lexer,
            current,
            interner,
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
            Token::Func | Token::Rec => self.parse_func(),
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

    fn parse_list_type(&mut self) -> ParseResult<Type> {
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

        Ok(Type::List(
            combine(&span, inner_type.get_span()),
            Box::new(inner_type.clone()),
        ))
    }

    fn parse_tuple_type(&mut self) -> ParseResult<Type> {
        let span = self.lexer.span();

        expect!(self.current()?, Token::Hash, span, "expected '#'");

        self.next();

        expect!(self.current()?, Token::LeftParen, span, "expected '('");

        self.next();

        let mut types = Vec::<Type>::new();
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

        let span = combine(&span, &self.lexer.span());

        if types.is_empty() {
            throw_error!(span.clone(), "tuple must contain at least one value");
        }

        Ok(Type::Tup(span, types))
    }

    fn parse_func_type(&mut self) -> ParseResult<Type> {
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

        let mut args = Vec::<Type>::new();
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
            let t = Type::convert_from(self.current()?.clone(), span).unwrap();
            self.next();
            t
        } else {
            self.parse_type()?
        };

        let end_span = self.lexer.span();

        Ok(Type::Func(combine(&span, &end_span), args, Box::new(return_type)))
    }

    fn parse_type(&mut self) -> ParseResult<Type> {
        match self.current()? {
            Token::LeftBrak => self.parse_list_type(),
            Token::Hash => self.parse_tuple_type(),
            Token::Func => self.parse_func_type(),
            _ => {
                let current = self.current()?.clone();
                let span = self.lexer.span();
                if let Some(t) = Type::convert_from(current, span.clone()) {
                    self.next();
                    Ok(t)
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

        let mut types = Vec::<Type>::new();

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
            name: self.interner.get_or_intern(enum_var_name),
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

        let span = combine(&span, &self.lexer.span());

        Ok(TopLevelExpr::EnumDef(Box::new(EnumDef {
            name: self.interner.get_or_intern(enum_name),
            variants: enum_vars,
            span: span.clone(),
            type_: Type::Unknown(span),
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
            name: self.interner.get_or_intern(field_name),
            type_: field_type,
            span,
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

        let span = combine(&span, &self.lexer.span());

        Ok(TopLevelExpr::StructDef(Box::new(StructDef {
            name: self.interner.get_or_intern(struct_name),
            fields: struct_fields,
            span: span.clone(),
            type_: Type::Unknown(span),
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
                name: self.interner.get_or_intern(param_name),
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
            "expected '{'"
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
            rec,
            name: self.interner.get_or_intern(func_name),
            closure: Closure {
                parameters: params.clone(),
                return_type: return_type.clone(),
                body,
                span: span.clone(),
                type_: Type::Func(
                    span.clone(),
                    params.into_iter().map(|p| p.type_.clone()).collect(),
                    return_type.into(),
                ),
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

        let mod_name = self.interner.get_or_intern(mod_name.clone());

        self.next();

        let mut import_symbols: Option<Vec<SymbolU32>> = None;

        if matches!(self.current, Ok(Token::LeftBrak)) {
            self.next();

            let mut imports = Vec::<SymbolU32>::new();

            while !matches!(self.current, Ok(Token::RightBrak)) {
                let Token::Ident(import) = expect!(
                    self.current()?,
                    Token::Ident(..),
                    self.lexer.span(),
                    "expected an identifier"
                ) else {
                    unreachable!()
                };

                imports.push(self.interner.get_or_intern(import.clone()));

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

        // Check mod_name & import_symbols

        Ok(TopLevelExpr::Use(Box::new(Use {
            module: mod_name,
            imports: import_symbols,
            span: combine(&span, &self.lexer.span()),
            type_: Type::Void(span),
        })))
    }

    fn parse_export(&mut self) -> ParseResult<TopLevelExpr> {
        todo!()
    }

    fn parse_standalone_expression(&mut self) -> ParseResult<Expr> {
        match self.current()? {
            Token::When => self.parse_when(),
            Token::If => self.parse_if(true),
            Token::While => self.parse_while(),
            Token::Match => self.parse_match(),
            _ => {
                let expr = self.parse_non_top_level_expression(false)?;

                expect!(
                    self.current()?,
                    Token::SemiColon,
                    self.lexer.span(),
                    "expected ';' after an expression"
                );

                self.next();

                Ok(expr)
            }
        }
    }

    fn parse_expression(&mut self) -> ParseResult<Expr> {
        self.parse_non_top_level_expression(true)
    }

    fn parse_non_top_level_expression(
        &mut self,
        include_if_when_while: bool,
    ) -> ParseResult<Expr> {
        if include_if_when_while {
            match self.current()? {
                Token::When => self.parse_when(),
                Token::If => self.parse_if(false),
                Token::While => self.parse_while(),
                Token::Match => self.parse_match(),
                _ => self.parse_assignment(),
            }
        } else {
            self.parse_assignment()
        }
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
            type_: Type::Unknown(span.clone()),
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

        let span = combine(&span, &self.lexer.span());

        let bin = Expr::binary(
            expr.clone(),
            bin_op,
            self.parse_expression()?,
            Type::Unknown(span.clone()),
        );

        Ok(Expr::AssignVar(Box::new(AssignVar {
            left: expr,
            value: bin,
            type_: Type::Unknown(span.clone()),
            span,
        })))
    }

    fn parse_or(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_and()?;

        while matches!(self.current, Ok(Token::Or)) {
            let span = self.lexer.span();

            self.next();

            expr = Expr::binary(
                expr,
                BinaryOp { kind: BinaryOpKind::Or, span: span.clone() },
                self.parse_and()?,
                Type::Bool(span),
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
                BinaryOp { kind: BinaryOpKind::And, span: span.clone() },
                self.parse_eq()?,
                Type::Bool(span),
            )
        }

        Ok(expr)
    }

    fn parse_eq(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_compare()?;

        while matches!(self.current, Ok(Token::NotEq) | Ok(Token::DEq)) {
            let span = self.lexer.span();
            let bin_op_kind = BinaryOpKind::from(self.current()?);
            let bin_op = BinaryOp { kind: bin_op_kind, span: span.clone() };

            self.next();

            expr = Expr::binary(
                expr,
                bin_op,
                self.parse_compare()?,
                Type::Bool(span),
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
            let bin_op = BinaryOp { kind: bin_op_kind, span: span.clone() };

            self.next();

            expr = Expr::binary(
                expr,
                bin_op,
                self.parse_term()?,
                Type::Bool(span),
            );
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> ParseResult<Expr> {
        let mut expr = self.parse_factor()?;

        while matches!(self.current, Ok(Token::Minus) | Ok(Token::Plus)) {
            let span = self.lexer.span();
            let bin_op_kind = BinaryOpKind::from(self.current()?);
            let bin_op = BinaryOp { kind: bin_op_kind, span: span.clone() };

            self.next();

            expr = Expr::binary(
                expr,
                bin_op,
                self.parse_factor()?,
                Type::Unknown(span),
            );
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
            let bin_op = BinaryOp { kind: bin_op_kind, span: span.clone() };

            self.next();

            expr = Expr::binary(
                expr,
                bin_op,
                self.parse_unary()?,
                Type::Unknown(span),
            );
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> ParseResult<Expr> {
        let expr = if matches!(self.current, Ok(Token::Not) | Ok(Token::Minus))
        {
            let span = self.lexer.span();
            let unary_op_kind = UnaryOpKind::from(self.current()?);
            let unary_op = UnaryOp { kind: unary_op_kind, span: span.clone() };
            let type_ = if let UnaryOpKind::NegBool = unary_op_kind {
                Type::Bool(span)
            } else {
                Type::Unknown(span)
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

        let module: SymbolU32;

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

        let constant = self.interner.get_or_intern(constant.clone());

        self.next();

        let span = combine(&span, &self.lexer.span());

        Ok(Expr::ModAccess(Box::new(ModAccess {
            module,
            constant,
            span: span.clone(),
            type_: Type::Unknown(span),
        })))
    }

    fn parse_struct_field_access(&mut self, left: Expr) -> ParseResult<Expr> {
        let span = self.lexer.span();

        let source: SymbolU32;

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

        let field = self.interner.get_or_intern(field.clone());

        self.next();

        let span = combine(&span, &self.lexer.span());

        Ok(Expr::StructFieldAccess(Box::new(StructFieldAccess {
            source,
            field,
            span: span.clone(),
            type_: Type::Unknown(span),
        })))
    }

    fn parse_enum_var_access(&mut self, left: Expr) -> ParseResult<Expr> {
        let span = self.lexer.span();

        let source: SymbolU32;

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

        let variant = self.interner.get_or_intern(variant.clone());

        self.next();

        let span = combine(&span, &self.lexer.span());

        Ok(Expr::EnumVarAccess(Box::new(EnumVarAccess {
            source,
            variant,
            span: span.clone(),
            type_: Type::Named(span, source),
        })))
    }

    fn parse_struct_init(&mut self, left: Expr) -> ParseResult<Expr> {
        let span = self.lexer.span();

        let struct_name: SymbolU32;

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
            name: struct_name,
            arguments: init_args,
            type_: Type::Named(span.clone(), struct_name),
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

        let name = self.interner.get_or_intern(name.clone());

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

        let span = combine(&span, &self.lexer.span());

        Ok(Expr::Index(Box::new(Index {
            source,
            index,
            span: span.clone(),
            type_: Type::Unknown(span),
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

        let span = combine(&span, &self.lexer.span());

        Ok(Expr::Call(Box::new(Call {
            callee,
            arguments: args,
            span: span.clone(),
            type_: Type::Unknown(span),
        })))
    }

    fn parse_primary(&mut self) -> ParseResult<Expr> {
        let span = self.lexer.span();

        match self.current {
            Ok(Token::True) => {
                self.next();
                Ok(Expr::Bool(Box::new(BoolLiteral {
                    value: true,
                    span: span.clone(),
                    type_: Type::Bool(span),
                })))
            }

            Ok(Token::False) => {
                self.next();
                Ok(Expr::Bool(Box::new(BoolLiteral {
                    value: false,
                    span: span.clone(),
                    type_: Type::Bool(span),
                })))
            }

            Ok(Token::Int(value)) => {
                self.next();
                Ok(Expr::Int(Box::new(IntLiteral {
                    value,
                    span: span.clone(),
                    type_: Type::Int(span),
                })))
            }

            Ok(Token::Float(value)) => {
                self.next();
                Ok(Expr::Float(Box::new(FloatLiteral {
                    value,
                    span: span.clone(),
                    type_: Type::Float(span),
                })))
            }

            Ok(Token::Str(ref value)) => {
                let value = self.interner.get_or_intern(value);
                self.next();
                Ok(Expr::Str(Box::new(StrLiteral {
                    value,
                    span: span.clone(),
                    type_: Type::Str(span),
                })))
            }

            Ok(Token::LeftBrak) => self.parse_list_literal(),

            Ok(Token::Hash) => self.parse_tuple_literal(),

            Ok(Token::Ident(ref ident)) => {
                let ident = self.interner.get_or_intern(ident);
                self.next();
                Ok(Expr::Ident(Box::new(Ident {
                    name: ident,
                    span: span.clone(),
                    type_: Type::Unknown(span),
                })))
            }

            Ok(Token::BackSlash) => self.parse_closure(),

            Ok(Token::LeftParen) => self.parse_group(),

            Ok(Token::If) => self.parse_if(false),

            Ok(Token::Let) => self.parse_let(),

            Ok(Token::Break) => self.parse_break(),

            Ok(Token::Return) => self.parse_return(),

            Ok(Token::Void) => {
                self.next();
                Ok(Expr::Void(Box::new(Void {
                    span: span.clone(),
                    type_: Type::Void(span),
                })))
            }

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

        expect!(
            self.current()?,
            Token::LeftParen,
            self.lexer.span(),
            "expected '('"
        );

        self.next();

        let expr = self.parse_expression()?;

        expect!(
            self.current()?,
            Token::RightParen,
            self.lexer.span(),
            "expected ')'"
        );

        self.next();

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

        let span = combine(&span, &self.lexer.span());

        Ok(Expr::Match(Box::new(Match {
            expression: expr,
            expressions: cases,
            span: span.clone(),
            type_: Type::Unknown(span),
        })))
    }

    fn parse_match_case(&mut self) -> ParseResult<MatchCase> {
        let span = self.lexer.span();

        let pattern = self.parse_pattern()?;

        let guard = if matches!(self.current, Ok(Token::If)) {
            self.next();

            expect!(
                self.current()?,
                Token::LeftParen,
                self.lexer.span(),
                "expected '('"
            );

            self.next();

            let expr = self.parse_expression()?;

            expect!(
                self.current()?,
                Token::RightParen,
                self.lexer.span(),
                "expected ')'"
            );

            self.next();

            Some(expr)
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
            span: combine(&span, &self.lexer.span()),
        })
    }

    fn parse_pattern(&mut self) -> ParseResult<Pattern> {
        let span = self.lexer.span();

        let mut pattern = match self.current()?.clone() {
            Token::Int(value) => {
                self.next();
                Pattern::Int(Box::new(IntLiteral {
                    value,
                    span: span.clone(),
                    type_: Type::Int(span),
                }))
            }

            Token::Float(value) => {
                self.next();
                Pattern::Float(Box::new(FloatLiteral {
                    value,
                    span: span.clone(),
                    type_: Type::Float(span),
                }))
            }

            Token::Str(value) => {
                self.next();
                let value = self.interner.get_or_intern(value);
                Pattern::Str(Box::new(StrLiteral {
                    value,
                    span: span.clone(),
                    type_: Type::Str(span),
                }))
            }

            Token::True => {
                self.next();
                Pattern::Bool(Box::new(BoolLiteral {
                    value: true,
                    span: span.clone(),
                    type_: Type::Bool(span),
                }))
            }

            Token::False => {
                self.next();
                Pattern::Bool(Box::new(BoolLiteral {
                    value: false,
                    span: span.clone(),
                    type_: Type::Bool(span),
                }))
            }

            Token::Underscore => {
                self.next();
                Pattern::Wildcard(span)
            }

            Token::LeftBrak => self.parse_list_pattern()?,

            Token::Hash => self.parse_tuple_pattern()?,

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

        if !or_patterns.is_empty() {
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
                name: self.interner.get_or_intern(ident),
                span: span.clone(),
                type_: Type::Unknown(span),
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
            source: self.interner.get_or_intern(ident),
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

        let name = self.interner.get_or_intern(name.clone());

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

        let variant = self.interner.get_or_intern(variant.clone());

        self.next();

        let span = combine(&span, &self.lexer.span());
        let source = self.interner.get_or_intern(ident);

        let enum_var_access = EnumVarAccess {
            source,
            variant,
            type_: Type::Named(span.clone(), source),
            span: span.clone(),
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

        let mut values = Vec::<Pattern>::new();
        while !matches!(self.current, Ok(Token::GT)) {
            values.push(self.parse_pattern()?);

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

        let span = combine(&span, &self.lexer.span());

        if values.is_empty() {
            throw_error!(span.clone(), "tuple must contain at least one value");
        }

        Ok(Pattern::Tuple(Box::new(TuplePattern { values, span })))
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

        Ok(Expr::Break(Box::new(Break {
            span: span.clone(),
            type_: Type::Void(span),
        })))
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

        let name = self.interner.get_or_intern(name.clone());

        self.next();

        let type_ = if !matches!(self.current, Ok(Token::Eq)) {
            self.parse_type()?
        } else {
            Type::Named(ident_span, name)
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

        expect!(
            self.current()?,
            Token::LeftParen,
            self.lexer.span(),
            "expected '('"
        );

        self.next();

        let cond = self.parse_expression()?;

        expect!(
            self.current()?,
            Token::RightParen,
            self.lexer.span(),
            "expected ')'"
        );

        self.next();

        let body = self.parse_curly_body()?;

        let span = combine(&span, &self.lexer.span());

        Ok(Expr::While(Box::new(While {
            condition: cond,
            body,
            span: span.clone(),
            type_: Type::Void(span),
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

        expect!(
            self.current()?,
            Token::LeftParen,
            self.lexer.span(),
            "expected '('"
        );

        self.next();

        let cond = self.parse_expression()?;

        expect!(
            self.current()?,
            Token::RightParen,
            self.lexer.span(),
            "expected ')'"
        );

        self.next();

        let then_body = if matches!(self.current, Ok(Token::Then)) {
            self.next();
            vec![self.parse_standalone_expression()?]
        } else {
            self.parse_curly_body()?
        };

        let else_body = if matches!(self.current, Ok(Token::Else)) {
            self.next();
            if matches!(self.current, Ok(Token::LeftBrace)) {
                Some(self.parse_curly_body()?)
            } else {
                Some(vec![self.parse_standalone_expression()?])
            }
        } else {
            None
        };

        let span = combine(&span, &self.lexer.span());

        Ok(Expr::When(Box::new(When {
            condition: cond,
            then: then_body,
            else_: else_body,
            span: span.clone(),
            type_: Type::Void(span),
        })))
    }

    fn parse_if(&mut self, standalone: bool) -> ParseResult<Expr> {
        let span = self.lexer.span();

        expect!(self.current()?, Token::If, self.lexer.span(), "expected 'if'");

        self.next();

        expect!(
            self.current()?,
            Token::LeftParen,
            self.lexer.span(),
            "expected '('"
        );

        self.next();

        let cond = self.parse_expression()?;

        expect!(
            self.current()?,
            Token::RightParen,
            self.lexer.span(),
            "expected ')'"
        );

        self.next();

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
        } else if standalone {
            vec![self.parse_standalone_expression()?]
        } else {
            vec![self.parse_expression()?]
        };

        let span = combine(&span, &self.lexer.span());

        Ok(Expr::If(Box::new(If {
            condition: cond,
            then: then_body,
            else_: else_body,
            span: span.clone(),
            type_: Type::Unknown(span),
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

        expect!(
            self.current()?,
            Token::RightParen,
            self.lexer.span(),
            "expected ')'"
        );

        self.next();

        Ok(Expr::Group(Box::new(Group {
            expression: expr,
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

        let span = combine(&span, &self.lexer.span());

        Ok(Expr::Closure(Box::new(Closure {
            parameters: params.clone(),
            return_type: return_type.clone(),
            body,
            span: span.clone(),
            type_: Type::Func(
                span,
                params.into_iter().map(|p| p.type_).collect(),
                return_type.into(),
            ),
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

        let span = combine(&span, &self.lexer.span());

        Ok(Expr::List(Box::new(List {
            elements,
            span: span.clone(),
            type_: Type::Unknown(span),
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

        let span = combine(&span, &self.lexer.span());

        if values.is_empty() {
            throw_error!(span.clone(), "tuple must contain at least one value")
        }

        Ok(Expr::Tuple(Box::new(Tuple {
            values,
            span: span.clone(),
            type_: Type::Unknown(span),
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
                    func.closure.body
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
        let mut interner = DefaultStringInterner::default();

        assert_eq!(test_parse("", &mut interner), Ok(vec![]));
        assert_eq!(test_parse("     ", &mut interner), Ok(vec![]));
        assert_eq!(test_parse("     \n", &mut interner), Ok(vec![]));
    }

    #[test]
    fn use_exprs() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: one_top(test_parse("use fmt;", &mut interner)),
            expected: TopLevelExpr::Use(Box::new(Use {
                module: interner.get_or_intern("fmt"),
                imports: None,
                span: 0..8,
                type_: Type::Void(0..3)
            }))
        );

        assert_eq!(
            result: one_error(test_parse("use", &mut interner)),
            expected: unexpected_end!(3..3)
        );

        assert_eq!(
            result: one_error(test_parse("use fmt", &mut interner)),
            expected: unexpected_end!(7..7)
        );

        assert_eq!(
            result: one_top(test_parse("use fmt [println, print];", &mut interner)),
            expected: TopLevelExpr::Use(Box::new(Use {
                module: interner.get_or_intern("fmt"),
                imports: Some(vec![interner.get_or_intern("println"), interner.get_or_intern("print")]),
                span: 0..25,
                type_: Type::Void(0..3)
            })),
        );

        assert_eq!(
            result: one_top(test_parse("use fmt [];", &mut interner)),
            expected: TopLevelExpr::Use(Box::new(Use {
                module: interner.get_or_intern("fmt"),
                imports: Some(vec![]),
                span: 0..11,
                type_: Type::Void(0..3)
            }))
        );

        assert_eq!(
            result: one_error(test_parse("use fmt [", &mut interner)),
            expected: unexpected_end!(9..9)
        );

        assert_eq!(
            result: one_error(test_parse("use fmt [println", &mut interner)),
            expected: unexpected_end!(16..16)
        );

        assert_eq!(
            result: multi_errors(test_parse("use fmt [;", &mut interner), 2),
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
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: one_top(test_parse("enum int_option { Some(int), None }", &mut interner)),
            expected: TopLevelExpr::EnumDef(Box::new(EnumDef {
                name: interner.get_or_intern("int_option"),
                variants: vec![
                    EnumVarDef {
                        name: interner.get_or_intern("Some"),
                        types: vec![Type::Int(23..26)],
                        span: 18..28
                    },
                    EnumVarDef {
                        name: interner.get_or_intern("None"), types: vec![], span: 29..35 }
                ],
                span: 0..35,
                type_: Type::Unknown(0..35)
            }))
        );

        assert_eq!(
            result: multi_errors(test_parse("enum int_option { Some(int) None }", &mut interner), 3),
            expected: vec![
                parse_error!("expected '}' after enum variants", 28..32),
                parse_error!("expected a top-level expression but found 'Ident(\"None\")'", 28..32),
                parse_error!("expected a top-level expression but found 'RightBrace'", 33..34),
            ]
        );

        assert_eq!(
            result: multi_errors(test_parse("enum;", &mut interner), 2),
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
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: one_top(test_parse("struct Person {}", &mut interner)),
            expected: TopLevelExpr::StructDef(Box::new(StructDef {
                name: interner.get_or_intern("Person"),
                fields: vec![],
                span: 0..16,
                type_: Type::Unknown(0..16)
            }))
        );

        assert_eq!(
            result: one_top(test_parse("struct Person { name str, age int }", &mut interner)),
            expected: TopLevelExpr::StructDef(Box::new(StructDef {
                name: interner.get_or_intern("Person"),
                fields: vec![
                    StructFieldDef {
                        name: interner.get_or_intern("name"),
                        type_: Type::Str(21..24),
                        span: 16..20
                    },
                    StructFieldDef {
                        name: interner.get_or_intern("age"),
                        type_: Type::Int(30..33),
                        span: 26..29
                    }
                ],
                span: 0..35,
                type_: Type::Unknown(0..35)
            }))
        );

        assert_eq!(
            result: multi_errors(test_parse("struct Person { name int;", &mut interner), 2),
            expected: vec![
                parse_error!("expected '}' after struct fields", 24..25),
                parse_error!("expected a top-level expression but found 'SemiColon'", 24..25)
            ],
        );

        assert_eq!(
            result: one_error(test_parse("struct Person", &mut interner)),
            expected: unexpected_end!(13..13),
        );
    }

    #[test]
    #[ignore = "generics is not implemented yet"]
    fn struct_with_generics() {}

    #[test]
    fn func_without_generics() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: one_top(test_parse("func main(args []str) void {}", &mut interner)),
            expected: TopLevelExpr::Func(Box::new(Func {
                rec: false,
                name: interner.get_or_intern("main"),
                closure: Closure {
                    parameters: vec![
                        FuncParam {
                            name: interner.get_or_intern("args"),
                            type_: Type::List(15..20, Box::new(Type::Str(17..20))),
                            span: 10..21
                        }
                    ],
                    return_type: Type::Void(22..26),
                    body: vec![],
                    span: 0..29,
                    type_: Type::Func(
                        0..29,
                        vec![Type::List(15..20, Box::new(Type::Str(17..20)))],
                        Box::new(Type::Void(22..26))
                    )
                },
                span: 0..29
            })),
        );

        assert_eq!(
            result: one_top(test_parse("func main() void do void;", &mut interner)),
            expected: TopLevelExpr::Func(Box::new(Func {
                rec: false,
                name: interner.get_or_intern("main"),
                closure: Closure {
                    parameters: vec![],
                    return_type: Type::Void(12..16),
                    body: vec![
                        Expr::Void(Box::new(Void {
                            span: 20..24,
                            type_: Type::Void(20..24)
                        }))
                    ],
                    span: 0..25,
                    type_: Type::Func(
                        0..25,
                        vec![],
                        Box::new(Type::Void(12..16))
                    )
                },
                span: 0..25
            }))
        );

        assert_eq!(
            result: one_error(test_parse("func main()", &mut interner)),
            expected: unexpected_end!(11..11)
        );

        assert_eq!(
            result: multi_errors(test_parse("func main() void }", &mut interner), 2),
            expected: vec![
                parse_error!("expected '{'", 17..18),
                parse_error!("expected a top-level expression but found 'RightBrace'", 17..18)
            ]
        );

        assert_eq!(
            result: multi_errors(test_parse("func main void", &mut interner), 2),
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
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: exprs(test_parse(
                "func main() void { 123; 1.23; \"Bello\"; true; [1, 2, 3]; #(\"bello\", 2.2, 3); some_var; }",
                &mut interner
            )),
            expected: vec![
                Expr::Int(Box::new(IntLiteral {
                    value: 123,
                    span: 19..22,
                    type_: Type::Int(19..22)
                })),
                Expr::Float(Box::new(FloatLiteral {
                    value: 1.23,
                    span: 24..28,
                    type_: Type::Float(24..28)
                })),
                Expr::Str(Box::new(StrLiteral {
                    value: interner.get_or_intern("Bello"),
                    span: 30..37,
                    type_: Type::Str(30..37)
                })),
                Expr::Bool(Box::new(BoolLiteral {
                    value: true,
                    span: 39..43,
                    type_: Type::Bool(39..43)
                })),
                Expr::List(Box::new(List {
                    elements: vec![
                        Expr::Int(Box::new(IntLiteral {
                            value: 1,
                            span: 46..47,
                            type_: Type::Int(46..47)
                        })),
                        Expr::Int(Box::new(IntLiteral {
                            value: 2,
                            span: 49..50,
                            type_: Type::Int(49..50)
                        })),
                        Expr::Int(Box::new(IntLiteral {
                            value: 3,
                            span: 52..53,
                            type_: Type::Int(52..53)
                        })),
                    ],
                    span: 45..55,
                    type_: Type::Unknown(45..55)
                })),
                Expr::Tuple(Box::new(Tuple {
                    values: vec![
                        Expr::Str(Box::new(StrLiteral {
                            value: interner.get_or_intern("bello"),
                            span: 58..65,
                            type_: Type::Str(58..65)
                        })),
                        Expr::Float(Box::new(FloatLiteral {
                            value: 2.2,
                            span: 67..70,
                            type_: Type::Float(67..70)
                        })),
                        Expr::Int(Box::new(IntLiteral {
                            value: 3,
                            span: 72..73,
                            type_: Type::Int(72..73)
                        })),
                    ],
                    span: 56..75,
                    type_: Type::Unknown(56..75)
                })),
                Expr::Ident(Box::new(Ident {
                    name: interner.get_or_intern("some_var"),
                    span: 76..84,
                    type_: Type::Unknown(76..84)
                }))
            ]
        );
    }

    #[test]
    fn enum_struct_init() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: exprs(test_parse(
                "func main() void { list:Cons(123) <. list:Cons(321) <. list:Nil; Person { age 18, name \"Nobu\", other stuff }; }",
                &mut interner
            )),
            expected: vec![Expr::Call(Box::new(Call {
                    callee: Expr::EnumVarAccess(Box::new(EnumVarAccess {
                        source: interner.get_or_intern("list"),
                        variant: interner.get_or_intern("Cons"),
                        span: 23..29,
                        type_: Type::Named(23..29, interner.get_or_intern("list"))
                    })),
                    arguments: vec![
                        Expr::Int(Box::new(IntLiteral {
                            value: 123,
                            span: 29..32,
                            type_: Type::Int(29..32)
                        })),
                        Expr::Call(Box::new(Call {
                            callee: Expr::EnumVarAccess(Box::new(EnumVarAccess {
                                source: interner.get_or_intern("list"),
                                variant: interner.get_or_intern("Cons"),
                                span: 41..47,
                                type_: Type::Named(41..47, interner.get_or_intern("list"))
                            })),
                            arguments: vec![
                                Expr::Int(Box::new(IntLiteral {
                                    value: 321,
                                    span: 47..50,
                                    type_: Type::Int(47..50)
                                })),
                                Expr::EnumVarAccess(Box::new(EnumVarAccess {
                                    source: interner.get_or_intern("list"),
                                    variant: interner.get_or_intern("Nil"),
                                    span: 59..64,
                                    type_: Type::Named(59..64, interner.get_or_intern("list"))
                                }))
                            ],
                            span: 46..64,
                            type_: Type::Unknown(46..64)
                        }))
                    ],
                    span: 28..64,
                    type_: Type::Unknown(28..64)
                })),
                Expr::StructInit(Box::new(StructInit {
                    name: interner.get_or_intern("Person"),
                    arguments: vec![
                        StructInitArg {
                            name: interner.get_or_intern("age"),
                            value: Expr::Int(Box::new(IntLiteral {
                                value: 18,
                                span: 78..80,
                                type_: Type::Int(78..80)
                            })),
                            span: 74..81
                        },
                        StructInitArg {
                            name: interner.get_or_intern("name"),
                            value: Expr::Str(Box::new(StrLiteral {
                                value: interner.get_or_intern("Nobu"),
                                span: 87..93,
                                type_: Type::Str(87..93)
                            })),
                            span: 82..94
                        },
                        StructInitArg {
                            name: interner.get_or_intern("other"),
                            value: Expr::Ident(Box::new(Ident {
                                name: interner.get_or_intern("stuff"),
                                span: 101..106,
                                type_: Type::Unknown(101..106)
                            })),
                            span: 95..108
                        }
                    ],
                    type_: Type::Named(72..73, interner.get_or_intern("Person")),
                    span: 72..109
                }))
            ]
        );
    }

    #[test]
    fn struct_field_access() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: exprs(test_parse("func main() void { person'name.length(); }", &mut interner)),
            expected: vec![
                Expr::Call(Box::new(Call {
                    callee: Expr::Ident(Box::new(Ident {
                        name: interner.get_or_intern("length"),
                        span: 31..37,
                        type_: Type::Unknown(31..37)
                    })),
                    arguments: vec![
                        Expr::StructFieldAccess(Box::new(StructFieldAccess {
                            source: interner.get_or_intern("person"),
                            field: interner.get_or_intern("name"),
                            span: 25..31,
                            type_: Type::Unknown(25..31)
                        }))
                    ],
                    span: 37..40,
                    type_: Type::Unknown(37..40)
                }))
            ]
        );
    }

    #[test]
    fn mod_access() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: exprs(test_parse("func main() void { fmt::println(\"Hello World\"); }", &mut interner)),
            expected: vec![
                Expr::Call(Box::new(Call {
                    callee: Expr::ModAccess(Box::new(ModAccess {
                        module: interner.get_or_intern("fmt"),
                        constant: interner.get_or_intern("println"),
                        span: 22..32,
                        type_: Type::Unknown(22..32)
                    })),
                    arguments: vec![
                        Expr::Str(Box::new(StrLiteral {
                            value: interner.get_or_intern("Hello World"),
                            span: 32..45,
                            type_: Type::Str(32..45)
                        }))
                    ],
                    span: 31..47,
                    type_: Type::Unknown(31..47),
                }))
            ]
        );
    }

    #[test]
    fn closure() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: exprs(test_parse("func main() void { \\(name str) str do name; }", &mut interner)),
            expected: vec![
                Expr::Closure(Box::new(Closure {
                    parameters: vec![
                        FuncParam {
                            name: interner.get_or_intern("name"),
                            type_: Type::Str(26..29),
                            span: 21..30
                        }
                    ],
                    return_type: Type::Str(31..34),
                    body: vec![
                        Expr::Ident(Box::new(Ident {
                            name: interner.get_or_intern("name"),
                            span: 38..42,
                            type_: Type::Unknown(38..42)
                        }))
                    ],
                    span: 19..43,
                    type_: Type::Func(
                        19..43,
                        vec![Type::Str(26..29)],
                        Box::new(Type::Str(31..34))
                    )
                }))
            ]
        );

        assert_eq!(
            result: exprs(test_parse("func main() void { \\(name str) str { name; }; }", &mut interner)),
            expected: vec![
                Expr::Closure(Box::new(Closure {
                    parameters: vec![
                        FuncParam {
                            name: interner.get_or_intern("name"),
                            type_: Type::Str(26..29),
                            span: 21..30,
                        }
                    ],
                    return_type: Type::Str(31..34),
                    body: vec![
                        Expr::Ident(Box::new(Ident {
                            name: interner.get_or_intern("name"),
                            span: 37..41,
                            type_: Type::Unknown(37..41)
                        }))
                    ],
                    span: 19..45,
                    type_: Type::Func(
                        19..45,
                        vec![Type::Str(26..29)],
                        Box::new(Type::Str(31..34))
                    )
                }))
            ]
        );
    }

    #[test]
    fn binary_unary() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: exprs(test_parse("func main() void { 1 + (1 - 1) / 1; }", &mut interner)),
            expected: vec![
                Expr::Binary(Box::new(Binary {
                    lhs: Expr::Int(Box::new(IntLiteral {
                        value: 1,
                        span: 19..20,
                        type_: Type::Int(19..20)
                    })),
                    rhs: Expr::Binary(Box::new(Binary {
                        lhs: Expr::Group(Box::new(Group {
                            expression: Expr::Binary(Box::new(Binary {
                                lhs: Expr::Int(Box::new(IntLiteral {
                                    value: 1,
                                    span: 24..25,
                                    type_: Type::Int(24..25)
                                })),
                                rhs: Expr::Int(Box::new(IntLiteral {
                                    value: 1,
                                    span: 28..29,
                                    type_: Type::Int(28..29)
                                })),
                                operator: BinaryOp {
                                    kind: BinaryOpKind::Sub,
                                    span: 26..27
                                },
                                type_: Type::Unknown(26..27),
                                span: 24..29,
                            })),
                            span: 23..32
                        })),
                        rhs: Expr::Int(Box::new(IntLiteral {
                            value: 1,
                            span: 33..34,
                            type_: Type::Int(33..34)
                        })),
                        operator: BinaryOp {
                            kind: BinaryOpKind::Div,
                            span: 31..32
                        },
                        type_: Type::Unknown(31..32),
                        span: 23..34,
                    })),
                    operator: BinaryOp {
                        kind: BinaryOpKind::Add,
                        span: 21..22
                    },
                    type_: Type::Unknown(21..22),
                    span: 19..34,
                }))
            ],
        );

        assert_eq!(
            result: exprs(test_parse("func main() void { not false; -(-23); }", &mut interner)),
            expected: vec![
                Expr::Unary(Box::new(Unary {
                    rhs: Expr::Bool(Box::new(BoolLiteral {
                        value: false,
                        span: 23..28,
                        type_: Type::Bool(23..28)
                    })),
                    operator: UnaryOp {
                        kind: UnaryOpKind::NegBool, span:
                        19..22
                    },
                    span: 19..28,
                    type_: Type::Bool(19..22)
                })),
                Expr::Unary(Box::new(Unary {
                    rhs: Expr::Group(Box::new(Group {
                        expression: Expr::Int(Box::new(IntLiteral {
                            value: -23,
                            span: 32..35,
                            type_: Type::Int(32..35)
                        })),
                        span: 31..37,
                    })),
                    operator: UnaryOp {
                        kind: UnaryOpKind::NegNum,
                        span: 30..31
                    },
                    type_: Type::Unknown(30..31),
                    span: 30..37,
                }))
            ]
        );
    }

    #[test]
    fn variables() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: exprs(test_parse("func main() void { let a int = 0; a = 123; let b = a; }", &mut interner)),
            expected: vec![
                Expr::DefVar(Box::new(DefVar {
                    name: interner.get_or_intern("a"),
                    value: Expr::Int(Box::new(IntLiteral {
                        value: 0,
                        span: 31..32,
                        type_: Type::Int(31..32)
                    })),
                    span: 19..33,
                    type_: Type::Int(25..28)
                })),
                Expr::AssignVar(Box::new(AssignVar {
                    left: Expr::Ident(Box::new(Ident {
                        name: interner.get_or_intern("a"),
                        span: 34..35,
                        type_: Type::Unknown(34..35)
                    })),
                    value: Expr::Int(Box::new(IntLiteral {
                        value: 123,
                        span: 38..41,
                        type_: Type::Int(38..41)
                    })),
                    type_: Type::Unknown(34..35),
                    span: 34..42,
                })),
                Expr::DefVar(Box::new(DefVar {
                    name: interner.get_or_intern("b"),
                    value: Expr::Ident(Box::new(Ident {
                        name: interner.get_or_intern("a"),
                        span: 51..52,
                        type_: Type::Unknown(51..52)
                    })),
                    span: 43..53,
                    type_: Type::Named(47..48, interner.get_or_intern("b"))
                }))
            ]
        );
    }

    #[test]
    fn index() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: exprs(test_parse("func main() void { list[0]; #(1, 2)[idx + 1]; }", &mut interner)),
            expected: vec![
                Expr::Index(Box::new(Index {
                    source: Expr::Ident(Box::new(Ident {
                        name: interner.get_or_intern("list"),
                        span: 19..23,
                        type_: Type::Unknown(19..23)
                    })),
                    index: Expr::Int(Box::new(IntLiteral {
                        value: 0,
                        span: 24..25,
                        type_: Type::Int(24..25)
                    })),
                    span: 23..27,
                    type_: Type::Unknown(23..27)
                })),
                Expr::Index(Box::new(Index {
                    source: Expr::Tuple(Box::new(Tuple {
                        values: vec![
                            Expr::Int(Box::new(IntLiteral {
                                value: 1,
                                span: 30..31,
                                type_: Type::Int(30..31)
                            })),
                            Expr::Int(Box::new(IntLiteral {
                                value: 2,
                                span: 33..34,
                                type_: Type::Int(33..34)
                            }))
                        ],
                        span: 28..36,
                        type_: Type::Unknown(28..36)
                    })),
                    index: Expr::Binary(Box::new(Binary {
                        lhs: Expr::Ident(Box::new(Ident {
                            name: interner.get_or_intern("idx"),
                            span: 36..39,
                            type_: Type::Unknown(36..39)
                        })),
                        rhs: Expr::Int(Box::new(IntLiteral {
                            value: 1,
                            span: 42..43,
                            type_: Type::Int(42..43)
                        })),
                        operator: BinaryOp {
                            kind: BinaryOpKind::Add,
                            span: 40..41
                        },
                        span: 36..43,
                        type_: Type::Unknown(40..41)
                    })),
                    span: 35..45,
                    type_: Type::Unknown(35..45)
                }))
            ]
        );
    }

    #[test]
    fn if_exprs() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: exprs(test_parse("func main() void { if (true) { void; } else { void; } }", &mut interner)),
            expected: vec![
                Expr::If(Box::new(If {
                    condition: Expr::Bool(Box::new(BoolLiteral {
                        value: true,
                        span: 23..27,
                        type_: Type::Bool(23..27)
                    })),
                    then: vec![
                        Expr::Void(Box::new(Void {
                            span: 31..35,
                            type_: Type::Void(31..35)
                        }))
                    ],
                    else_: vec![
                        Expr::Void(Box::new(Void {
                            span: 46..50,
                            type_: Type::Void(46..50)
                        }))
                    ],
                    span: 19..55,
                    type_: Type::Unknown(19..55)
                }))
            ]
        );

        assert_eq!(
            result: exprs(test_parse("func main() void { if (true) then void; else void; }", &mut interner)),
            expected: vec![
                Expr::If(Box::new(If {
                    condition: Expr::Bool(Box::new(BoolLiteral {
                        value: true,
                        span: 23..27,
                        type_: Type::Bool(23..27)
                    })),
                    then: vec![
                        Expr::Void(Box::new(Void {
                            span: 34..38,
                            type_: Type::Void(34..38)
                        }))
                    ],
                    else_: vec![
                        Expr::Void(Box::new(Void {
                            span: 45..49,
                            type_: Type::Void(45..49)
                        }))
                    ],
                    span: 19..52,
                    type_: Type::Unknown(19..52)
                }))
            ]
        );

        assert_eq!(
            result: exprs(test_parse("func main() void { let a = if (true) then void; else void; }", &mut interner)),
            expected: vec![
                Expr::DefVar(Box::new(DefVar {
                    name: interner.get_or_intern("a"),
                    value: Expr::If(Box::new(If {
                        condition: Expr::Bool(Box::new(BoolLiteral {
                            value: true,
                            span: 31..35,
                            type_: Type::Bool(31..35)
                        })),
                        then: vec![
                            Expr::Void(Box::new(Void {
                                span: 42..46,
                                type_: Type::Void(42..46)
                            }))
                        ],
                        else_: vec![
                            Expr::Void(Box::new(Void {
                                span: 53..57,
                                type_: Type::Void(53..57)
                            }))
                        ],
                        span: 27..58,
                        type_: Type::Unknown(27..58)
                    })),
                    span: 19..58,
                    type_: Type::Named(23..24, interner.get_or_intern("a"))
                }))
            ]
        );
    }

    #[test]
    fn when_exprs() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: exprs(test_parse("func main() void { when (true) { void; } }", &mut interner)),
            expected: vec![
                Expr::When(Box::new(When {
                    condition: Expr::Bool(Box::new(BoolLiteral {
                        value: true,
                        span: 25..29,
                        type_: Type::Bool(25..29)
                    })),
                    then: vec![
                        Expr::Void(Box::new(Void {
                            span: 33..37,
                            type_: Type::Void(33..37)
                        }))
                    ],
                    else_: None,
                    span: 19..42,
                    type_: Type::Void(19..42)
                }))
            ]
        );
        assert_eq!(
            result: exprs(test_parse("func main() void { when (true) then void; }", &mut interner)),
            expected: vec![
                Expr::When(Box::new(When {
                    condition: Expr::Bool(Box::new(BoolLiteral {
                        value: true,
                        span: 25..29,
                        type_: Type::Bool(25..29)
                    })),
                    then: vec![
                        Expr::Void(Box::new(Void {
                            span: 36..40,
                            type_: Type::Void(36..40)
                        }))
                    ],
                    else_: None,
                    span: 19..43,
                    type_: Type::Void(19..43)
                }))
            ]
        );

        assert_eq!(
            result: exprs(test_parse("func main() void { when (true) { void; } else { void; } }", &mut interner)),
            expected: vec![
                Expr::When(Box::new(When {
                    condition: Expr::Bool(Box::new(BoolLiteral {
                        value: true,
                        span: 25..29,
                        type_: Type::Bool(25..29)
                    })),
                    then: vec![
                        Expr::Void(Box::new(Void {
                            span: 33..37,
                            type_: Type::Void(33..37)
                        }))
                    ],
                    else_: Some(vec![
                        Expr::Void(Box::new(Void {
                            span: 48..52,
                            type_: Type::Void(48..52)
                        }))
                    ]),
                    span: 19..57,
                    type_: Type::Void(19..57)
                }))
            ]
        );

        assert_eq!(
            result: exprs(test_parse("func main() void { when (true) then void; else void; }", &mut interner)),
            expected: vec![
                Expr::When(Box::new(When {
                    condition: Expr::Bool(Box::new(BoolLiteral {
                        value: true,
                        span: 25..29,
                        type_: Type::Bool(25..29)
                    })),
                    then: vec![
                        Expr::Void(Box::new(Void {
                            span: 36..40,
                            type_: Type::Void(36..40)
                        }))
                    ],
                    else_: Some(vec![
                        Expr::Void(Box::new(Void {
                            span: 47..51,
                            type_: Type::Void(47..51)
                        }))
                    ]),
                    span: 19..54,
                    type_: Type::Void(19..54)
                }))
            ]
        );
    }

    #[test]
    fn while_exprs() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: exprs(test_parse("func main() void { while (true) { \"hello\"; } }", &mut interner)),
            expected: vec![
                Expr::While(Box::new(While {
                    condition: Expr::Bool(Box::new(BoolLiteral {
                        value: true,
                        span: 26..30,
                        type_: Type::Bool(26..30)
                    })),
                    body: vec![
                        Expr::Str(Box::new(StrLiteral {
                            value: interner.get_or_intern("hello"),
                            span: 34..41,
                            type_: Type::Str(34..41)
                        }))
                    ],
                    span: 19..46,
                    type_: Type::Void(19..46)
                }))
            ]
        );
    }

    #[test]
    fn break_return() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: exprs(test_parse("func main() void { break; return void; return 123; }", &mut interner)),
            expected: vec![
                Expr::Break(Box::new(Break {
                    span: 19..24,
                    type_: Type::Void(19..24)
                })),
                Expr::Return(Box::new(Return {
                    value: Expr::Void(Box::new(Void {
                        span: 33..37,
                        type_: Type::Void(33..37)
                    })),
                    span: 26..38,
                })),
                Expr::Return(Box::new(Return {
                    value: Expr::Int(Box::new(IntLiteral {
                        value: 123,
                        span: 46..49,
                        type_: Type::Int(46..49)
                    })),
                    span: 39..50
                }))
            ]
        );
    }

    #[test]
    fn match_exprs() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: exprs(test_parse(
r#"func main() void {
    match (#(n % 3, n % 5)) {
        #(0, 0) then "fizz buzz",
        #(0, _) then "fizz",
        #(_, 0) { "buzz"; },
        _ { n.to_str(); }
    }
}"#,
            &mut interner)),
            expected: vec![
                Expr::Match(Box::new(Match {
                    expression: Expr::Tuple(Box::new(Tuple {
                        values: vec![
                            Expr::Binary(Box::new(Binary {
                                lhs: Expr::Ident(Box::new(Ident {
                                    name: interner.get_or_intern("n"),
                                    span: 32..33,
                                    type_: Type::Unknown(32..33)
                                })),
                                rhs: Expr::Int(Box::new(IntLiteral {
                                    value: 3,
                                    span: 36..37,
                                    type_: Type::Int(36..37)
                                })),
                                operator: BinaryOp {
                                    kind: BinaryOpKind::Rem,
                                    span: 34..35
                                },
                                span: 32..37,
                                type_: Type::Unknown(34..35)
                            })),
                            Expr::Binary(Box::new(Binary {
                                lhs: Expr::Ident(Box::new(Ident {
                                    name: interner.get_or_intern("n"),
                                    span: 39..40,
                                    type_: Type::Unknown(39..40)
                                })),
                                rhs: Expr::Int(Box::new(IntLiteral {
                                    value: 5,
                                    span: 43..44,
                                    type_: Type::Int(43..44)
                                })),
                                operator: BinaryOp {
                                    kind: BinaryOpKind::Rem,
                                    span: 41..42
                                },
                                span: 39..44,
                                type_: Type::Unknown(41..42)
                            }))
                        ],
                        span: 30..46,
                        type_: Type::Unknown(30..46)
                    })),
                    expressions: vec![
                        MatchCase {
                            pattern: Pattern::Tuple(Box::new(TuplePattern {
                                values: vec![
                                    Pattern::Int(Box::new(IntLiteral {
                                        value: 0,
                                        span: 59..60,
                                        type_: Type::Int(59..60)
                                    })),
                                    Pattern::Int(Box::new(IntLiteral {
                                        value: 0,
                                        span: 62..63,
                                        type_: Type::Int(62..63)
                                    }))
                                ],
                                span: 57..69
                            })),
                            guard: None,
                            body: vec![
                                Expr::Str(Box::new(StrLiteral {
                                    value: interner.get_or_intern("fizz buzz"),
                                    span: 70..81,
                                    type_: Type::Str(70..81)
                                }))
                            ],
                            span: 57..82
                        },
                        MatchCase {
                            pattern: Pattern::Tuple(Box::new(TuplePattern {
                                values: vec![
                                    Pattern::Int(Box::new(IntLiteral {
                                        value: 0,
                                        span: 93..94,
                                        type_: Type::Int(93..94)
                                    })),
                                    Pattern::Wildcard(96..97)
                                ],
                                span: 91..103
                            })),
                            guard: None,
                            body: vec![
                                Expr::Str(Box::new(StrLiteral {
                                    value: interner.get_or_intern("fizz"),
                                    span: 104..110,
                                    type_: Type::Str(104..110)
                                }))
                            ],
                            span: 91..111
                        },
                        MatchCase {
                            pattern: Pattern::Tuple(Box::new(TuplePattern {
                                values: vec![
                                    Pattern::Wildcard(122..123),
                                    Pattern::Int(Box::new(IntLiteral {
                                        value: 0,
                                        span: 125..126,
                                        type_: Type::Int(125..126)
                                    }))
                                ],
                                span: 120..129
                            })),
                            guard: None,
                            body: vec![
                                Expr::Str(Box::new(StrLiteral {
                                    value: interner.get_or_intern("buzz"),
                                    span: 130..136,
                                    type_: Type::Str(130..136)
                                }))
                            ],
                            span: 120..140
                        },
                        MatchCase {
                            pattern: Pattern::Wildcard(149..150),
                            guard: None,
                            body: vec![
                                Expr::Call(Box::new(Call {
                                    callee: Expr::Ident(Box::new(Ident {
                                        name: interner.get_or_intern("to_str"),
                                        span: 155..161,
                                        type_: Type::Unknown(155..161)
                                    })),
                                    arguments: vec![
                                        Expr::Ident(Box::new(Ident {
                                            name: interner.get_or_intern("n"),
                                            span: 153..154,
                                            type_: Type::Unknown(153..154,)
                                        }))
                                    ],
                                    span: 161..164,
                                    type_: Type::Unknown(161..164)
                                }))
                            ],
                            span: 149..172
                        }
                    ],
                    span: 23..174,
                    type_: Type::Unknown(23..174)
                })),
            ]
        );

        assert_eq!(
            result: exprs(test_parse(
r#"func main() void {
    match (void) {}
}
"#,
            &mut interner)),
            expected: vec![
                Expr::Match(Box::new(Match {
                    expression: Expr::Void(Box::new(Void {
                        span: 30..34,
                        type_: Type::Void(30..34)
                    })),
                    expressions: vec![],
                    span: 23..40,
                    type_: Type::Unknown(23..40)
                }))
            ]
        );
    }

    #[test]
    fn match_expr_guards() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: exprs(test_parse(
r#"func main() void {
    match (void) {
        n if (n != 0) { void; },
        [a, b] if (a == b) { void; }
    }
}"#,
            &mut interner)),
            expected: vec![
                Expr::Match(Box::new(Match {
                    expression: Expr::Void(Box::new(Void {
                        span: 30..34,
                        type_: Type::Void(30..34)
                    })),
                    expressions: vec![
                        MatchCase {
                            pattern: Pattern::Ident(Box::new(Ident {
                                name: interner.get_or_intern("n"),
                                span: 46..47,
                                type_: Type::Unknown(46..47)
                            })),
                            guard: Some(Expr::Binary(Box::new(Binary {
                                lhs: Expr::Ident(Box::new(Ident {
                                    name: interner.get_or_intern("n"),
                                    span: 52..53,
                                    type_: Type::Unknown(52..53)
                                })),
                                rhs: Expr::Int(Box::new(IntLiteral {
                                    value: 0,
                                    span: 57..58,
                                    type_: Type::Int(57..58)
                                })),
                                operator: BinaryOp {
                                    kind: BinaryOpKind::NEq,
                                    span: 54..56
                                },
                                span: 52..58,
                                type_: Type::Bool(54..56)
                            }))),
                            body: vec![
                                Expr::Void(Box::new(Void {
                                    span: 62..66,
                                    type_: Type::Void(62..66)
                                }))
                            ],
                            span: 46..70
                        },
                        MatchCase {
                            pattern: Pattern::List(Box::new(ListPattern {
                                elements: vec![
                                    Pattern::Ident(Box::new(Ident {
                                        name: interner.get_or_intern("a"),
                                        span: 80..81,
                                        type_: Type::Unknown(80..81)
                                    })),
                                    Pattern::Ident(Box::new(Ident {
                                        name: interner.get_or_intern("b"),
                                        span: 83..84,
                                        type_: Type::Unknown(83..84)
                                    }))
                                ],
                                span: 79..88
                            })),
                            guard: Some(Expr::Binary(Box::new(Binary {
                                lhs: Expr::Ident(Box::new(Ident {
                                    name: interner.get_or_intern("a"),
                                    span: 90..91,
                                    type_: Type::Unknown(90..91)
                                })),
                                rhs: Expr::Ident(Box::new(Ident {
                                    name: interner.get_or_intern("b"),
                                    span: 95..96,
                                    type_: Type::Unknown(95..96)
                                })),
                                operator: BinaryOp {
                                    kind: BinaryOpKind::Eq,
                                    span: 92..94
                                },
                                span: 90..96,
                                type_: Type::Bool(92..94)
                            }))),
                            body: vec![
                                Expr::Void(Box::new(Void {
                                    span: 100..104,
                                    type_: Type::Void(100..104)
                                }))
                            ],
                            span: 79..113
                        }
                    ],
                    span: 23..115,
                    type_: Type::Unknown(23..115)
                })),
            ]
        );
    }

    #[test]
    fn type_exprs() {
        let mut interner = DefaultStringInterner::default();

        assert_eq!(
            result: exprs(test_parse(
r#"func main() void {
    let a int = 0;
    let b float = 0.0;
    let c str = "hello";
    let d bool = true;
    let e void = void;
    let f [][]int = [[0, 0]];
    let g #(#(int, int), str) = #(#(1, 1), "hello");
    let h func(int) int = 0;
}"#,
            &mut interner)),
            expected: vec![
                Expr::DefVar(Box::new(DefVar {
                    name: interner.get_or_intern("a"),
                    value: Expr::Int(Box::new(IntLiteral {
                        value: 0,
                        span: 35..36,
                        type_: Type::Int(35..36)
                    })),
                    type_: Type::Int(29..32),
                    span: 23..37
                })),
                Expr::DefVar(Box::new(DefVar {
                    name: interner.get_or_intern("b"),
                    value: Expr::Float(Box::new(FloatLiteral {
                        value: 0.0,
                        span: 56..59,
                        type_: Type::Float(56..59)
                    })),
                    type_: Type::Float(48..53),
                    span: 42..60
                })),
                Expr::DefVar(Box::new(DefVar {
                    name: interner.get_or_intern("c"),
                    value: Expr::Str(Box::new(StrLiteral {
                        value: interner.get_or_intern("hello"),
                        span: 77..84,
                        type_: Type::Str(77..84)
                    })),
                    type_: Type::Str(71..74),
                    span: 65..85
                })),
                Expr::DefVar(Box::new(DefVar {
                    name: interner.get_or_intern("d"),
                    value: Expr::Bool(Box::new(BoolLiteral {
                        value: true,
                        span: 103..107,
                        type_: Type::Bool(103..107)
                    })),
                    type_: Type::Bool(96..100),
                    span: 90..108
                })),
                Expr::DefVar(Box::new(DefVar {
                    name: interner.get_or_intern("e"),
                    value: Expr::Void(Box::new(Void {
                        span: 126..130,
                        type_: Type::Void(126..130)
                    })),
                    type_: Type::Void(119..123),
                    span: 113..131
                })),
                Expr::DefVar(Box::new(DefVar {
                    name: interner.get_or_intern("f"),
                    value: Expr::List(Box::new(List {
                        elements: vec![
                            Expr::List(Box::new(List {
                                elements: vec![
                                    Expr::Int(Box::new(IntLiteral {
                                        value: 0,
                                        span: 154..155,
                                        type_: Type::Int(154..155)
                                    })),
                                    Expr::Int(Box::new(IntLiteral {
                                        value: 0,
                                        span: 157..158,
                                        type_: Type::Int(157..158)
                                    }))
                                ],
                                span: 153..160,
                                type_: Type::Unknown(153..160)
                            }))
                        ],
                        span: 152..161,
                        type_: Type::Unknown(152..161)
                    })),
                    type_: Type::List(142..149, Box::new(Type::List(144..149, Box::new(Type::Int(146..149))))),
                    span: 136..161
                })),
                Expr::DefVar(Box::new(DefVar {
                    name: interner.get_or_intern("g"),
                    value: Expr::Tuple(Box::new(Tuple {
                        values: vec![
                            Expr::Tuple(Box::new(Tuple {
                                values: vec![
                                    Expr::Int(Box::new(IntLiteral {
                                        value: 1,
                                        span: 198..199,
                                        type_: Type::Int(198..199)
                                    })),
                                    Expr::Int(Box::new(IntLiteral {
                                        value: 1,
                                        span: 201..202,
                                        type_: Type::Int(201..202)
                                    }))
                                ],
                                span: 196..204,
                                type_: Type::Unknown(196..204)
                            })),
                            Expr::Str(Box::new(StrLiteral {
                                value: interner.get_or_intern("hello"),
                                span: 205..212,
                                type_: Type::Str(205..212)
                            }))
                        ],
                        span: 194..214,
                        type_: Type::Unknown(194..214)
                    })),
                    type_: Type::Tup(
                        172..193,
                        vec![
                            Type::Tup(
                                174..186,
                                vec![
                                    Type::Int(176..179),
                                    Type::Int(181..184)
                                ]),
                                Type::Str(187..190)
                        ]),
                    span: 166..214
                })),
                Expr::DefVar(Box::new(DefVar {
                    name: interner.get_or_intern("h"),
                    value: Expr::Int(Box::new(IntLiteral {
                        value: 0,
                        span: 241..242,
                        type_: Type::Int(241..242)
                    })),
                    type_:
                        Type::Func(
                            225..240,
                            vec![Type::Int(230..233)],
                            Box::new(Type::Int(235..238))
                        ),
                    span: 219..243
                })),
            ]
        );
    }
}
