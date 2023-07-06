use crate::{
    ast::Pattern::{self, *},
    parse::{PError, PResult, Parser},
    parser,
    tks::TokenKind::*,
};

impl<'src> Parser<'src> {
    pub fn pat_collection(&mut self) -> PResult<Pattern> {
        self.skip_token(OpenBracket)?;
        let pats = self.separated(parser!(pattern))?;
        self.skip_token(CloseBracket)?;
        Ok(PatCollection(pats))
    }
    pub fn pat_struct(&mut self) -> PResult<Pattern> {
        self.skip_token(OpenBrace)?;
        let pats = self.separated(|p| match p.current() {
            Identifier => {
                let id = p.identifier()?;
                if p.current() == Equals {
                    p.bump()?;
                    let pat = p.pattern()?;
                    Ok(PatValued(id, Box::new(pat)))
                } else {
                    Ok(PatIdentifier(id))
                }
            }
            DotDot => {
                p.bump()?;
                Ok(PatDotDot)
            }
            _ => Err(PError::NoMatch),
        })?;
        self.skip_token(CloseBrace)?;
        Ok(PatStruct(pats))
    }
    pub fn pat_ident(&mut self) -> PResult<Pattern> {
        Ok(PatIdentifier(self.identifier()?))
    }
    pub fn pat_tuple(&mut self) -> PResult<Pattern> {
        self.skip_token(OpenParen)?;
        let pats = self.separated(parser!(pattern))?;
        self.skip_token(CloseParen)?;
        Ok(PatTuple(pats))
    }
}
