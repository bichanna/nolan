use std::fs;

use logos::{Lexer, Logos};

use crate::ast::*;
use crate::error::{combine, LexError, ParseError, ParseErrorKind};
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
            Token::Func => self.parse_func(),
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

        let inner_type = self.parse_type()?;

        Ok(Spanned(
            Type::List(Box::new(inner_type.clone())),
            combine(&span, &inner_type.1),
        ))
    }

    fn parse_tuple_type(&mut self) -> ParseResult<SpannedType> {
        let span = self.lexer.span();

        expect!(self.current()?, Token::LT, span, "expected '<'");

        self.next();

        let mut types = Vec::<SpannedType>::new();
        while !matches!(self.current, Ok(Token::GT)) {
            types.push(self.parse_type()?);

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

    fn parse_func(&mut self) -> ParseResult<TopLevelExpr> {
        todo!()
    }

    fn parse_use(&mut self) -> ParseResult<TopLevelExpr> {
        todo!()
    }

    fn parse_export(&mut self) -> ParseResult<TopLevelExpr> {
        todo!()
    }
}
