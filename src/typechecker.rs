use crate::ast::TopLevelExpr;
use crate::error::TypeCheckError;

type CheckResult<T> = Result<T, Vec<TypeCheckError>>;

struct Checker {
    ast: Vec<TopLevelExpr>,
    index: usize,
}

pub fn check(ast: Vec<TopLevelExpr>) -> CheckResult<Vec<TopLevelExpr>> {
    let mut checker = Checker::new(ast);

    let errs = checker.check();

    if let Err(errs) = errs {
        Err(errs)
    } else {
        Ok(checker.ast)
    }
}

impl Checker {
    fn new(ast: Vec<TopLevelExpr>) -> Self {
        Self { ast, index: 0 }
    }

    #[inline]
    fn current(&self) -> Option<&TopLevelExpr> {
        self.ast.get(self.index)
    }

    #[inline]
    fn next(&mut self) {
        if self.ast.len() > self.index {
            self.index += 1;
        }
    }

    fn check(&mut self) -> CheckResult<()> {
        todo!()
    }
}
