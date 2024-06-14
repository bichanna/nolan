use std::collections::HashMap;
use string_interner::symbol::SymbolU32;
use string_interner::DefaultStringInterner;

use crate::ast::TopLevelExpr;
use crate::error::{Span, Spanned, TypeCheckError};
use crate::types::Type;

type TypeCheckResult = Result<(), TypeCheckError>;

enum Either<L, R> {
    Left(L),
    Right(R),
}

struct TypeEnv<'a> {
    types: HashMap<SymbolU32, Either<Type, Vec<Span>>>,
    interner: &'a mut DefaultStringInterner,
}

struct TypeChecker<'a> {
    ast: Vec<TopLevelExpr>,
    index: usize,
    type_env: TypeEnv<'a>,
}

type AST = Vec<TopLevelExpr>;

pub fn type_check(
    ast: AST,
    interner: &mut DefaultStringInterner,
) -> Result<AST, Vec<TypeCheckError>> {
    let mut type_checker = TypeChecker::new(ast, interner);
    if let Err(errs) = type_checker.check() {
        Err(errs)
    } else {
        Ok(type_checker.ast)
    }
}

impl<'a> TypeChecker<'a> {
    fn new(
        ast: Vec<TopLevelExpr>,
        interner: &'a mut DefaultStringInterner,
    ) -> Self {
        Self { ast, index: 0, type_env: TypeEnv::new(interner) }
    }

    #[inline]
    fn current(&self) -> &TopLevelExpr {
        self.ast.get(self.index).unwrap()
    }

    #[inline]
    fn next(&mut self) -> bool {
        if self.ast.len() - 1 == self.index {
            false
        } else {
            self.index += 1;
            true
        }
    }

    fn check(&mut self) -> Result<(), Vec<TypeCheckError>> {
        let mut errs = Vec::new();

        loop {
            if let Err(err) = self.check_top_level_expr() {
                errs.push(err);
            }

            if !self.next() {
                break;
            }
        }

        if errs.is_empty() {
            Ok(())
        } else {
            Err(errs)
        }
    }

    fn check_top_level_expr(&mut self) -> TypeCheckResult {
        todo!()
    }
}

impl<'a> TypeEnv<'a> {
    fn new(interner: &'a mut DefaultStringInterner) -> Self {
        Self { types: HashMap::new(), interner }
    }

    #[inline]
    fn force_resolve(&self, symbol: SymbolU32) -> &str {
        self.interner.resolve(symbol).unwrap()
    }

    #[inline]
    fn intern<T>(&mut self, s: T) -> SymbolU32
    where
        T: AsRef<str>,
    {
        self.interner.get_or_intern(s)
    }

    #[inline]
    fn has_type(&mut self, symbol: &SymbolU32) -> bool {
        self.types.contains_key(symbol)
    }

    fn insert_type(
        &mut self,
        symbol: SymbolU32,
        type_: Type,
    ) -> TypeCheckResult {
        if self.types.contains_key(&symbol) {
            Err(Spanned(
                format!("{} is already defined", self.force_resolve(symbol)),
                type_.get_span().clone(),
            ))
        } else {
            self.types.insert(symbol, Either::Left(type_));
            Ok(())
        }
    }

    fn not_defined_yet(
        &mut self,
        symbol: SymbolU32,
        span: Span,
    ) -> TypeCheckResult {
        if self.types.contains_key(&symbol) {
            if let Either::Right(spans) = self.types.get_mut(&symbol).unwrap() {
                spans.push(span);
            }
            Ok(())
        } else {
            self.types.insert(symbol, Either::Right(vec![span]));
            Ok(())
        }
    }

    fn has_unknown_types(&self) -> Option<Vec<TypeCheckError>> {
        let mut errs = Vec::new();

        for (key, val) in self.types.iter() {
            if let Either::Right(spans) = val {
                let type_name = self.force_resolve(*key);
                for span in spans {
                    errs.push(Spanned(
                        format!("{} is not defined", type_name),
                        span.clone(),
                    ));
                }
            }
        }

        if !errs.is_empty() {
            Some(errs)
        } else {
            None
        }
    }
}
