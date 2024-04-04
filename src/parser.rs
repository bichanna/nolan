use std::fs;

use logos::{Lexer, Logos};

use crate::ast::*;
use crate::error::lex::LexError;
use crate::error::parse::{ParseError, ParseErrorKind};
use crate::error::{SourcePath, Spanned};
use crate::lexer::Token;
use crate::types::Type;

type ParseResult<T> = Result<T, ParseError>;

macro_rules! throw_error {
    ($span: expr, $msg: expr, $($f: expr),*) => {
        return Err(Spanned(ParseErrorKind::ParseErr(format!($msg, $(f),*)), $span));
    };

    ($span: expr, $msg: expr) => {
        return Err(Spanned(ParseErrorKind::ParseErr($msg.to_string())));
    }
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

pub fn parse(source: SourcePath) -> Result<Module, Vec<ParseError>> {
    let module_name = source
        .0
        .file_stem()
        .unwrap()
        .to_str()
        .expect("invalid file stem")
        .to_string();
    let src = fs::read_to_string(source.clone())
        .expect(&format!("Invalid path '{}'", source));

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
        name: module_name,
        path: source,
        expressions: exprs?,
        type_: Type::Void,
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

    fn parse(
        mut self,
    ) -> (SourcePath, Result<Vec<TopLevelExpr>, Vec<ParseError>>) {
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

    fn parse_top_level_expression(&mut self) -> ParseResult<TopLevelExpr> {
        todo!()
    }
}
