use std::fs;

use logos::{Lexer, Logos};

use crate::error::lex::LexError;
use crate::error::parse::{ParseError, ParseErrorKind};
use crate::error::{SourcePath, Spanned};
use crate::lexer::Token;
use crate::parser::ast::*;
use crate::types::Type;

pub mod ast;

/// A recursive-descent parser that converts tokenized Nolan source code into an AST.
struct Parser<'a> {
    source: SourcePath,
    lexer: Lexer<'a, Token>,
    ast: Vec<TopLevelExpr>,
    current: Result<Token, LexError>,
}

pub fn parse(source: SourcePath) -> Result<Module, ParseError> {
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
            return Err(Spanned(
                ParseErrorKind::LexErr(err.clone()),
                lexer.span(),
            ));
        }
    } else {
        return Ok(Module::new(module_name, source));
    }

    let current = current.unwrap();
    let (source, exprs) =
        Parser { source, lexer, ast: Vec::new(), current }.parse();

    Ok(Module {
        name: module_name,
        path: source,
        expressions: exprs,
        type_: Type::Void,
    })
}

impl<'a> Parser<'a> {
    fn parse(self) -> (SourcePath, Vec<TopLevelExpr>) {
        todo!()
    }
}
