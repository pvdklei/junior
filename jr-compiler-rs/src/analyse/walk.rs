use crate::ast::*;

#[allow(unused_variables)]
pub trait Visitor: Sized {
    fn visit_file(&mut self, stmts: &mut [Statement]) {
        visit_file(self, stmts);
    }
    fn visit_statement(&mut self, stmt: &mut Statement) {
        visit_statement(self, stmt);
    }
    fn visit_pattern(&mut self, pat: &mut Pattern) {
        visit_pattern(self, pat);
    }
    fn visit_expression(&mut self, expr: &mut Expression) {
        visit_expression(self, expr);
    }
    fn visit_type(&mut self, ty: &mut Type) {
        visit_type(self, ty);
    }
    fn visit_modifier(&mut self, m: &mut Modifier) {
        visit_modifier(self, m);
    }
    fn visit_identifier(&mut self, id: &mut Identifier) {}
    fn visit_operator(&mut self, ty: &mut Operator) {}
}

pub fn visit_file<V: Visitor>(v: &mut V, stmts: &mut [Statement]) {
    for stmt in stmts {
        v.visit_statement(stmt);
    }
}

pub fn visit_statement<V: Visitor>(v: &mut V, stmt: &mut Statement) {
    match stmt {
        StmtLet(pat, expr) => {
            v.visit_pattern(pat);
            v.visit_expression(expr);
        }
        StmtExpr(expr) => v.visit_expression(expr),
        StmtStruct(id, ty) | StmtEnum(id, ty) | StmtType(id, ty) => {
            v.visit_identifier(id);
            v.visit_type(ty);
        }
        StmtIf(expr, expr1) => {
            v.visit_expression(expr);
            v.visit_expression(expr1);
        }
    }
}

pub fn visit_expression<V: Visitor>(v: &mut V, expr: &mut Expression) {
    match expr {
        ExprIdentifier(id) => v.visit_identifier(id),
        ExprDo(stmts) => {
            for stmt in stmts {
                v.visit_statement(stmt);
            }
        }
        ExprCollection(exprs) => {
            for expr in exprs {
                v.visit_expression(expr);
            }
        }
        ExprTuple(exprs) => {
            for expr in exprs {
                v.visit_expression(expr);
            }
        }
        ExprIfElse(a) => {
            let (expr1, expr2, expr3) = a.as_mut();
            v.visit_expression(expr1);
            v.visit_expression(expr2);
            v.visit_expression(expr3);
        }
        ExprMatch(expr, aspat, arms) => {
            v.visit_expression(expr);
            if let Some(aspat) = aspat {
                v.visit_pattern(aspat);
            }
            for (pat, expr) in arms {
                v.visit_pattern(pat);
                v.visit_expression(expr);
            }
        }
        ExprUnaryOperation(op, expr) => {
            v.visit_operator(op);
            v.visit_expression(expr);
        }
        ExprStruct(attrs) => {
            for (id, val) in attrs {
                v.visit_identifier(id);
                if let Some(expr) = val {
                    v.visit_expression(expr);
                }
            }
        }
        ExprFn(pat, retty, expr) => {
            v.visit_pattern(pat);
            if let Some(ty) = retty {
                v.visit_type(ty);
            }
            v.visit_expression(expr);
        }
        ExprModified(expr, mods) => {
            v.visit_expression(expr);
            for m in mods {
                v.visit_modifier(m);
            }
        }
        ExprWithMeta { expr, .. } => v.visit_expression(expr),
        ExprKewword(_) | ExprNumber(_) | ExprString(_) => {}
    }
}

pub fn visit_pattern<V: Visitor>(v: &mut V, pat: &mut Pattern) {
    match pat {
        PatCollection(pats) | PatTuple(pats) | PatStruct(pats) => {
            for pat in pats {
                v.visit_pattern(pat)
            }
        }
        // PatGuarded(pat, expr) => {
        //     v.visit_pattern(pat);
        //     v.visit_expression(expr);
        // }
        PatOr(ors) => {
            for or in ors {
                v.visit_pattern(or)
            }
        }
        PatTyped(pat, ty) => {
            v.visit_pattern(pat);
            v.visit_type(ty);
        }
        PatValued(id, pat) => {
            v.visit_identifier(id);
            v.visit_pattern(pat);
        }
        PatIdentifier(id) => v.visit_identifier(id),
        PatLabeled(id, pat) => {
            v.visit_identifier(id);
            v.visit_pattern(pat);
        }
        PatNumber(_) | PatString(_) | PatWildcard | PatDotDot => {}
    }
}

pub fn visit_type<V: Visitor>(v: &mut V, ty: &mut Type) {
    match ty {
        TypeCollection(ty) => v.visit_type(ty),
        TypeTuple(tys) => {
            for ty in tys {
                v.visit_type(ty);
            }
        }
        TypeStruct(attrs) => {
            for (id, ty) in attrs {
                v.visit_identifier(id);
                v.visit_type(ty);
            }
        }
        TypeEnum(attrs) => {
            for (label, ty) in attrs {
                v.visit_identifier(label);
                v.visit_type(ty);
            }
        }
        TypeIdentifier(id) => v.visit_identifier(id),
        TypeWithGenerics(ty, tys) => {
            v.visit_type(ty);
            for ty in tys {
                v.visit_type(ty);
            }
        }
        // TypeWithPathSegment(id, ty) => {
        //     v.visit_identifier(id);
        //     v.visit_type(ty);
        // }
        TypeKeyword(_) => {}
        TypeFn(_, _) => todo!(),
    }
}

pub fn visit_modifier<V: Visitor>(v: &mut V, m: &mut Modifier) {
    match m {
        ModAttribute(expr) | ModCall(expr) => v.visit_expression(expr),
        ModBinaryOperation(op, expr) => {
            v.visit_operator(op);
            v.visit_expression(expr);
        }
        ModGenerics(tys) => {
            for ty in tys {
                v.visit_type(ty);
            }
        }
        ModKeywordAttribute(_) | ModQuestion => {}
        ModPipe(_) => todo!(),
    }
}
