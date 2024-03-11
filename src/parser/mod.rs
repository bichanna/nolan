pub mod ast;
pub mod error;
use std::fs;

use crate::error::report_error;
use crate::lexer::Token;
use crate::parser::ast::{ExprNode, StmtNode};
pub use crate::parser::error::ParseError;
use logos::{Lexer, Logos};

struct Parser<'a> {
    filename: &'a str,
    lexer: Lexer<'a, Token>,
    ast: Vec<StmtNode>,
    current: Token,
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

    let current = current.unwrap().unwrap();
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
    fn declaration(&mut self) -> Result<StmtNode, ParseError> {
        todo!()
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
}
