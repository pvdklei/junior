use crate::ast::*;
use crate::parse::PResult;
use crate::parse::Parser;
use crate::parser;
use crate::tks::{CloseBrace, CloseBracket, Colon, OpenBrace, OpenBracket};

impl<'src> Parser<'src> {
    pub fn type_struct(&mut self) -> PResult<Type> {
        self.skip_token(OpenBrace)?;
        let attributes = self.separated(|p| {
            let id = p.identifier()?;
            p.skip_token(Colon)?;
            let ty = p.typ()?;
            Ok((id, ty))
        })?;
        self.skip_token(CloseBrace)?;
        Ok(TypeStruct(attributes))
    }
    pub fn type_enum(&mut self) -> PResult<Type> {
        self.skip_token(OpenBrace)?;
        let attributes = self.separated(|p| {
            let id = p.identifier()?;
            let ty = p.typ()?;
            Ok((id, ty))
        })?;
        self.skip_token(CloseBrace)?;
        Ok(TypeEnum(attributes))
    }
    pub fn type_tuple(&mut self) -> PResult<Type> {
        self.skip_token(OpenBrace)?;
        let tys = self.separated(parser!(typ))?;
        self.skip_token(CloseBrace)?;
        Ok(TypeTuple(tys))
    }
    pub fn type_collection(&mut self) -> PResult<Type> {
        self.skip_token(OpenBracket)?;
        let ty = self.typ()?;
        self.skip_token(CloseBracket)?;
        Ok(TypeCollection(Box::new(ty)))
    }
    pub fn type_ident(&mut self) -> PResult<Type> {
        Ok(TypeIdentifier(self.identifier()?))
    }
}
