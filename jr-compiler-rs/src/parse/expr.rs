use crate::ast::*;
use crate::parse::PError;
use crate::parse::PResult;
use crate::parse::Parser;
use crate::parser;
use crate::tks::TokenKind::*;

impl<'src> Parser<'src> {
    pub fn expr_number(&mut self) -> PResult<Expression> {
        if self.current() != Number {
            return Err(PError::NoMatch);
        }
        let ret = Ok(ExprNumber(self.current_string()));
        self.bump()?;
        ret
    }
    pub fn expr_string(&mut self) -> PResult<Expression> {
        if self.current() != Quoted {
            return Err(PError::NoMatch);
        }
        let ret = Ok(ExprNumber(self.current_string()));
        self.bump()?;
        ret
    }
    pub fn expr_tuple(&mut self) -> PResult<Expression> {
        self.skip_token(OpenParen)?;
        let exprs = self.separated(parser!(expression))?;
        self.skip_token(CloseParen)?;
        Ok(ExprTuple(exprs))
    }
    pub fn expr_list(&mut self) -> PResult<Expression> {
        self.skip_token(OpenParen)?;
        let exprs = self.separated(parser!(expression))?;
        self.skip_token(CloseBracket)?;
        Ok(ExprCollection(exprs))
    }
    pub fn expr_struct(&mut self) -> PResult<Expression> {
        self.skip_token(OpenBrace)?;
        let attrs = self.separated(|p| {
            let ident = p.identifier()?;
            if let Ok(()) = p.skip_token(Equals) {
                Ok((ident, Some(p.expression()?)))
            } else {
                Ok((ident, None))
            }
        })?;
        self.skip_token(CloseBrace)?;
        Ok(ExprStruct(attrs))
    }
    pub fn expr_match(&mut self) -> PResult<Expression> {
        self.skip_token(Match)?;
        let expr = self.expression()?;
        self.skip_separators1()?;
        let arms = self.separated1(|p| {
            let pat = p.pattern()?;
            p.skip_token(Then)?;
            let expr = p.expression()?;
            Ok((pat, expr))
        })?;
        Ok(ExprMatch(Box::new(expr), None, arms))
    }
    pub fn expr_ifelse(&mut self) -> PResult<Expression> {
        self.skip_token(If)?;
        let cond = self.expression()?;
        self.skip_separators1()?;
        let ifs = self.separated1(parser!(statement))?;
        self.skip_token(Else)?;
        let elses = self.separated1(parser!(statement))?;
        self.skip_token(End)?;
        Ok(ExprIfElse(Box::new((cond, ExprDo(ifs), ExprDo(elses)))))
    }
    pub fn expr_unop(&mut self) -> PResult<Expression> {
        let op = self.operator()?;
        let expr = self.expression()?;
        Ok(ExprUnaryOperation(op, Box::new(expr)))
    }
    pub fn expr_do(&mut self) -> PResult<Expression> {
        self.skip_token(Do)?;
        let stmts = self.separated(parser!(statement))?;
        self.skip_token(End)?;
        Ok(ExprDo(stmts))
    }
    pub fn expr_id(&mut self) -> PResult<Expression> {
        if self.current() != Identifier {
            return Err(PError::NoMatch);
        }
        let id = self.current_string();
        self.bump()?;
        Ok(ExprIdentifier(id))
    }
}
