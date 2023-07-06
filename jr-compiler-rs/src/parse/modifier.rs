use crate::{
    ast::Modifier::{self, *},
    parse::{PResult, Parser},
    tks::TokenKind::*,
};

impl<'src> Parser<'src> {
    pub fn mod_attr(&mut self) -> PResult<Modifier> {
        self.skip_token(Dot)?;
        let expr = self.expression()?;
        Ok(ModAttribute(expr))
    }
    pub fn mod_pipe(&mut self) -> PResult<Modifier> {
        self.skip_token(Or)?;
        let expr = self.expression()?;
        Ok(ModPipe(expr))
    }
    pub fn mod_call(&mut self) -> PResult<Modifier> {
        let expr = self.expression()?;
        Ok(ModCall(expr))
    }
    pub fn mod_binop(&mut self) -> PResult<Modifier> {
        let op = self.operator()?;
        let expr = self.expression()?;
        Ok(ModBinaryOperation(op, expr))
    }
    pub fn mod_question(&mut self) -> PResult<Modifier> {
        self.skip_token(Question)?;
        Ok(ModQuestion)
    }
}
