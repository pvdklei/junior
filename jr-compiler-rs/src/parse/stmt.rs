use super::{PResult, Parser};
use crate::ast::Statement;
use crate::tks::TokenKind::*;
use crate::{ast::*, parser};

impl<'src> Parser<'src> {
    pub fn stmt_expr(&mut self) -> PResult<Statement> {
        let expr = self.expression()?;
        Ok(StmtExpr(expr))
    }
    pub fn stmt_let(&mut self) -> PResult<Statement> {
        self.skip_token(Let)?;
        let pat = self.pattern()?;
        self.skip_token(Equals)?;
        let expr = self.expression()?;
        Ok(StmtLet(pat, expr))
    }
    pub fn stmt_struct(&mut self) -> PResult<Statement> {
        self.skip_token(Struct)?;
        let id = self.identifier()?;
        let ty = self.type_struct()?;
        Ok(StmtStruct(id, ty))
    }
    pub fn stmt_enum(&mut self) -> PResult<Statement> {
        self.skip_token(Enum)?;
        let id = self.identifier()?;
        let ty = self.type_enum()?;
        Ok(StmtEnum(id, ty))
    }
    pub fn stmt_type(&mut self) -> PResult<Statement> {
        self.skip_token(Type)?;
        let id = self.identifier()?;
        let ty = self.typ()?;
        Ok(StmtType(id, ty))
    }
    pub fn stmt_if(&mut self) -> PResult<Statement> {
        self.skip_token(If)?;
        let cond = self.expression()?;
        self.skip_separators1()?;
        let then = self.separated1(parser!(statement))?;
        if let Ok(()) = self.skip_token(Else) {
            let else_ = self.separated1(parser!(statement))?;
            Ok(StmtExpr(ExprIfElse(Box::new((
                cond,
                ExprDo(then),
                ExprDo(else_),
            )))))
        } else {
            Ok(StmtIf(cond, ExprDo(then)))
        }
    }
}
