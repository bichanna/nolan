use crate::ast::*;
use crate::error::{Span, Spanned, TypeCheckError};
use crate::types::Type;
use std::collections::HashMap;
use string_interner::symbol::SymbolU32;
use string_interner::DefaultStringInterner;

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
    errors: Vec<TypeCheckError>,
}

type Ast = Vec<TopLevelExpr>;

pub fn type_check(
    ast: Ast,
    interner: &mut DefaultStringInterner,
) -> Result<Ast, Vec<TypeCheckError>> {
    let mut type_checker = TypeChecker::new(ast, interner);

    type_checker.check();

    if type_checker.errors.is_empty() {
        Ok(type_checker.ast)
    } else {
        Err(type_checker.errors)
    }
}

impl<'a> TypeChecker<'a> {
    fn new(
        ast: Vec<TopLevelExpr>,
        interner: &'a mut DefaultStringInterner,
    ) -> Self {
        Self {
            ast,
            index: 0,
            type_env: TypeEnv::new(interner),
            errors: Vec::new(),
        }
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

    fn check(&mut self) {
        loop {
            self.check_top_level_expr();

            if !self.next() {
                break;
            }
        }

        if let Some(mut unknown_type_errs) = self.type_env.has_unknown_types() {
            self.errors.append(&mut unknown_type_errs);
        }
    }

    fn check_top_level_expr(&mut self) {
        match self.current() {
            TopLevelExpr::EnumDef(_) => self.check_enum_def(),
            TopLevelExpr::StructDef(_) => self.check_struct_def(),
            TopLevelExpr::Module(_) => self.check_module(),
            TopLevelExpr::Func(_) => self.check_func(),
            TopLevelExpr::Use(_) => self.check_use(),
            TopLevelExpr::Export(_) => self.check_export(),
        }
    }

    fn check_export(&mut self) {
        let TopLevelExpr::Export(export) = self.current().clone() else {
            unreachable!()
        };

        for Spanned(symbol, span) in export.symbols {
            if !self.type_env.has_type(&symbol) {
                self.type_env.not_defined_yet(symbol, span);
            }
        }
    }

    fn check_use(&mut self) {
        // Nothing to check
    }

    fn check_func(&mut self) {
        let TopLevelExpr::Func(func) = self.current().clone() else {
            unreachable!()
        };

        if func.rec {
            // ??
        }

        self.check_closure(func.closure);
    }

    fn check_module(&mut self) {
        let TopLevelExpr::Module(module) = self.current().clone() else {
            unreachable!()
        };

        let mod_name = module.name;
        let mod_type = module.type_;

        if let Err(err) = self.type_env.insert_type(mod_name, mod_type) {
            self.errors.push(err);
        }

        let mut type_checker =
            TypeChecker::new(module.expressions, self.type_env.interner);

        type_checker.check();

        self.errors.append(&mut type_checker.errors);
    }

    fn check_struct_def(&mut self) {
        let TopLevelExpr::StructDef(struct_def) = self.current().clone() else {
            unreachable!()
        };

        let struct_name = struct_def.name;
        let struct_span = struct_def.span;

        if let Err(err) = self
            .type_env
            .insert_type(struct_name, Type::Named(struct_span, struct_name))
        {
            self.errors.push(err);
        }

        for field in &struct_def.fields {
            self.check_type(&field.type_);
        }
    }

    fn check_enum_def(&mut self) {
        let TopLevelExpr::EnumDef(enum_def) = self.current().clone() else {
            unreachable!()
        };

        let enum_name = enum_def.name;
        let enum_span = enum_def.span;

        if let Err(err) = self
            .type_env
            .insert_type(enum_name, Type::Named(enum_span, enum_name))
        {
            self.errors.push(err);
        }

        for variant in &enum_def.variants {
            for typ in &variant.types {
                self.check_type(typ);
            }
        }
    }

    fn check_type(&mut self, typ: &Type) {
        match typ {
            Type::Int(_)
            | Type::Float(_)
            | Type::Str(_)
            | Type::Bool(_)
            | Type::Void(_)
            | Type::Unknown(_) => {}

            Type::List(_, typ) => self.check_type(typ),

            Type::Tup(_, types) => {
                for typ in types {
                    self.check_type(typ);
                }
            }

            Type::Func(_, at, rt) => {
                for typ in at {
                    self.check_type(typ);
                }

                self.check_type(rt);
            }

            Type::Named(span, type_name) => {
                if !self.type_env.has_type(type_name) {
                    self.type_env.not_defined_yet(*type_name, span.clone());
                }
            }
        }
    }

    fn check_expr(&mut self, expr: Expr) -> Option<Type> {
        match expr {
            Expr::Int(int_literal) => Some(int_literal.get_type().clone()),
            Expr::Float(float) => Some(float.get_type().clone()),
            Expr::Str(str_literal) => Some(str_literal.get_type().clone()),
            Expr::Bool(bool_literal) => Some(bool_literal.get_type().clone()),
            Expr::List(list) => self.check_list(*list),
            Expr::Tuple(tup) => self.check_tuple(*tup),
            _ => None,
        }
    }

    fn check_tuple(
        &mut self,
        Tuple { values, span: _, type_ }: Tuple,
    ) -> Option<Type> {
        let Type::Tup(_, ref types) = type_ else {
            self.errors.push(Spanned(
                format!(
                    "expected tuple type but got {}",
                    &type_.display(self.type_env.interner)
                ),
                type_.get_span().clone(),
            ));
            return None;
        };

        for (value, tt) in values.into_iter().zip(types) {
            let t = self.check_expr(value)?;
            if !t.ignore_span_equal(tt) {
                self.errors.push(Spanned(
                    format!(
                        "expected {} but got {}",
                        tt.display(self.type_env.interner),
                        t.display(self.type_env.interner),
                    ),
                    t.get_span().clone(),
                ));
                return None;
            }
        }

        Some(type_)
    }

    fn check_list(
        &mut self,
        List { elements, span, type_: _ }: List,
    ) -> Option<Type> {
        if !elements.is_empty() {
            let elems = elements
                .into_iter()
                .map(|elem| self.check_expr(elem))
                .collect::<Vec<Option<Type>>>();

            for elem in &elems {
                if elem.is_none() {
                    return None;
                }
            }

            let elems = elems
                .into_iter()
                .map(|elem| elem.unwrap())
                .collect::<Vec<Type>>();

            let first = elems.first().unwrap();
            for elem in &elems {
                if !elem.ignore_span_equal(first) {
                    self.errors.push(Spanned(
                        format!(
                            "expected type of {}",
                            first.display(self.type_env.interner)
                        ),
                        elem.get_span().clone(),
                    ));
                    return None;
                }
            }

            Some(Type::List(span, Box::new(first.clone())))
        } else {
            Some(Type::List(span.clone(), Box::new(Type::Unknown(span))))
        }
    }

    fn check_closure(
        &mut self,
        Closure { parameters, return_type, mut body, span, type_: _ }: Closure,
    ) -> Option<Type> {
        for func_param in &parameters {
            self.check_type(&func_param.type_);
        }

        self.check_type(&return_type);

        let closure_type = Type::Func(
            span,
            parameters.iter().map(|param| param.type_.clone()).collect(),
            return_type.clone().into(),
        );

        match &return_type {
            Type::Void(_) => Some(closure_type),

            _ if body.is_empty() => {
                self.errors.push(Spanned(
                    format!(
                        "expected {} but the body is empty",
                        return_type.display(self.type_env.interner)
                    ),
                    0..0,
                ));

                None
            }

            _ => {
                let last_expr = body.pop().unwrap();
                if !self.check_expr(last_expr)?.ignore_span_equal(&return_type)
                {
                    return None;
                }

                for expr in body {
                    self.check_expr(expr);
                }

                Some(closure_type)
            }
        }
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
    fn has_type(&mut self, symbol: &SymbolU32) -> bool {
        self.types.contains_key(symbol)
    }

    fn insert_type(
        &mut self,
        symbol: SymbolU32,
        type_: Type,
    ) -> TypeCheckResult {
        if let std::collections::hash_map::Entry::Vacant(e) =
            self.types.entry(symbol)
        {
            e.insert(Either::Left(type_));
            Ok(())
        } else {
            Err(Spanned(
                format!("{} is already defined", self.force_resolve(symbol)),
                type_.get_span().clone(),
            ))
        }
    }

    fn not_defined_yet(&mut self, symbol: SymbolU32, span: Span) {
        if let std::collections::hash_map::Entry::Vacant(e) =
            self.types.entry(symbol)
        {
            e.insert(Either::Right(vec![span]));
        } else if let Either::Right(spans) =
            self.types.get_mut(&symbol).unwrap()
        {
            spans.push(span);
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
