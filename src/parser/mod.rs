pub mod ast;

use crate::lexer::Token;
use logos::Lexer;

struct Parser<'a> {
    lexer: Lexer<'a, Token>,
}
