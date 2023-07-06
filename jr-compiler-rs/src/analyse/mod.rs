#![allow(dead_code)]
pub mod walk;
use std::ops::Deref;

use crate::ast::{Ast, Expression, Identifier, Modifier, Operator, Pattern, Statement, Type};

use self::walk::Visitor;
enum NodeKind {
    Stmt(Statement),
    Expr(Expression),
    Mod(Modifier),
    Pat(Pattern),
    Ast(Ast),
}
struct Node {
    kind: Box<NodeKind>,
    hash: SomeHash,
}
type SomeHash = usize;

#[derive(Copy, Clone)]
struct Ptr<T>(*const T);
impl<T> Deref for Ptr<T> {
    type Target = T;
    fn deref(&self) -> &Self::Target {
        unsafe { &*self.0 }
    }
}

pub struct HashCollector {
    hashes: Vec<SomeHash>,
    hashcounter: SomeHash,
}

impl Visitor for HashCollector {
    fn visit_file(&mut self, stmts: &mut [Statement]) {
        walk::visit_file(self, stmts);
    }

    fn visit_statement(&mut self, stmt: &mut Statement) {
        walk::visit_statement(self, stmt);
    }

    fn visit_pattern(&mut self, pat: &mut Pattern) {
        walk::visit_pattern(self, pat);
    }

    fn visit_expression(&mut self, expr: &mut Expression) {
        walk::visit_expression(self, expr);
        match expr {
            Expression::ExprModified(_, _) => todo!(),
            Expression::ExprIfElse(_) => todo!(),
            Expression::ExprMatch(_, _, _) => todo!(),
            Expression::ExprFn(_, _, _) => todo!(),
            Expression::ExprDo(_) => todo!(),
            Expression::ExprNumber(_) => todo!(),
            Expression::ExprString(_) => todo!(),
            Expression::ExprCollection(_) => todo!(),
            Expression::ExprTuple(_) => todo!(),
            Expression::ExprStruct(_) => todo!(),
            Expression::ExprUnaryOperation(_, _) => todo!(),
            Expression::ExprIdentifier(_) => todo!(),
            Expression::ExprKewword(_) => todo!(),
            Expression::ExprWithMeta { .. } => todo!(),
        }
    }

    fn visit_type(&mut self, ty: &mut Type) {
        walk::visit_type(self, ty);
    }

    fn visit_modifier(&mut self, m: &mut Modifier) {
        walk::visit_modifier(self, m);
    }

    fn visit_identifier(&mut self, _id: &mut Identifier) {}

    fn visit_operator(&mut self, _ty: &mut Operator) {}
}

pub struct NodeCollector {
    nodes: Vec<Ptr<Node>>,
}
