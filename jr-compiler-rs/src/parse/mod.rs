mod expr;
mod modifier;
mod pattern;
mod stmt;
mod typ;

use crate::{
    ast::{
        Ast, ExprModified, Expression, Identifier, Modifier, Operator, PatWildcard, Pattern,
        Statement, Type, TypeFn,
    },
    tks::{
        TokenKind::{self, *},
        Tokens,
    },
};

#[derive(Debug)]
pub enum PError {
    EndOfSrc,
    NoMatch,
}
pub type PResult<T> = Result<T, PError>;
type Parse<'src, T> = fn(&mut Parser<'src>) -> PResult<T>;

pub struct Parser<'src> {
    tks: Tokens<'src>,
    cursor: usize,
    srcidx: usize,
    checkpoints: Vec<usize>,
}

#[macro_export]
macro_rules! parsers {
    ($( $parse:ident ),* ) => {&[
        $( {
            |x| x.$parse()
        } ),*
    ]};
}
#[macro_export]
macro_rules! parser {
    ($parse:ident) => {{
        |x| x.$parse()
    }};
}
pub use parser;
pub use parsers;

impl<'src> Parser<'src> {
    pub fn new(tks: Tokens<'src>) -> Self {
        Self {
            tks,
            cursor: 0,
            checkpoints: Vec::new(),
            srcidx: 0,
        }
    }
    pub fn file(&mut self) -> PResult<Ast> {
        let mut ast = Vec::new();
        self.skip_separators()?;
        loop {
            match self.statement() {
                Ok(stmt) => ast.push(stmt),
                Err(PError::EndOfSrc) => break,
                Err(err) => return Err(err),
            }
            self.skip_separators()?;
        }
        Ok(ast)
    }
    pub fn statement(&mut self) -> PResult<Statement> {
        match self.current() {
            Let => self.stmt_let(),
            Struct => self.stmt_struct(),
            Type => self.stmt_type(),
            Enum => self.stmt_enum(),
            If => self.stmt_if(),
            _ => self.stmt_expr(),
        }
    }
    pub fn expression(&mut self) -> PResult<Expression> {
        let expr = match self.current() {
            If => self.expr_ifelse(),
            Match => self.expr_match(),
            OpenBrace => self.expr_struct(),
            OpenBracket => self.expr_list(),
            OpenParen => self.expr_tuple(),
            Identifier => self.expr_id(),
            Quoted => self.expr_string(),
            Number => self.expr_number(),
            Do => self.expr_do(),
            _ => self.expr_unop(),
        }?;
        let mut modifiers = Vec::new();
        while let Ok(m) = self.modifier() {
            modifiers.push(m);
        }
        if modifiers.len() > 0 {
            Ok(ExprModified(Box::new(expr), modifiers))
        } else {
            Ok(expr)
        }
    }
    pub fn typ(&mut self) -> PResult<Type> {
        let ty = match self.current() {
            OpenBracket => self.type_collection()?,
            OpenBrace => self.type_struct()?,
            OpenParen => self.type_tuple()?,
            Struct => {
                self.bump()?;
                self.type_struct()?
            }
            Enum => {
                self.bump()?;
                self.type_enum()?
            }
            Identifier => self.type_ident()?,
            _ => return Err(PError::NoMatch),
        };
        if let Ok(()) = self.skip_token(Rarrow) {
            self.bump()?;
            Ok(TypeFn(Box::new(ty), Box::new(self.typ()?)))
        } else {
            Ok(ty)
        }
    }
    pub fn pattern(&mut self) -> PResult<Pattern> {
        match self.current() {
            OpenBrace => self.pat_struct(),
            OpenBracket => self.pat_collection(),
            OpenParen => self.pat_tuple(),
            Identifier => self.pat_ident(),
            Underscore => Ok(PatWildcard),
            _ => Err(PError::NoMatch),
        }
    }
    pub fn identifier(&mut self) -> PResult<Identifier> {
        if self.current() == Identifier {
            let val = self.current_string();
            self.bump()?;
            Ok(val)
        } else {
            Err(PError::NoMatch)
        }
    }
    pub fn modifier(&mut self) -> PResult<Modifier> {
        match self.current() {
            Dot => self.mod_attr(),
            Or => self.mod_pipe(),
            Question => self.mod_question(),
            tkn if token_is_operator(tkn) => self.mod_binop(),
            _ => self.mod_call(),
        }
    }
    pub fn operator(&mut self) -> PResult<Operator> {
        if token_is_operator(self.current()) {
            Ok(self.current())
        } else {
            Err(PError::NoMatch)
        }
    }
}

// Utility functions
impl<'src> Parser<'src> {
    fn checkpoint(&mut self) {
        self.checkpoints.push(self.cursor)
    }
    fn finish(&mut self) {
        self.checkpoints.pop();
    }
    fn reset(&mut self) {
        self.cursor = *self.checkpoints.last().unwrap();
    }
    // gets the slice contained by the current token
    fn current_string(&self) -> String {
        let len = self.tks[self.cursor].len;
        self.tks.src[self.srcidx..self.srcidx + len as usize]
            .iter()
            .take_while(|c| !c.is_whitespace())
            .collect::<String>()
    }
    fn bump(&mut self) -> PResult<()> {
        self.srcidx += self.tks[self.cursor].len as usize;
        self.cursor += 1;
        if self.tks.len() <= self.cursor {
            Err(PError::EndOfSrc)
        } else {
            Ok(())
        }
    }
    fn skip_separators(&mut self) -> PResult<()> {
        while self.current() == Separator {
            match self.bump() {
                Err(err @ PError::EndOfSrc) => return Err(err),
                Ok(()) => (),
                Err(_) => unreachable!(),
            }
        }
        Ok(())
    }
    fn skip_separators1(&mut self) -> PResult<()> {
        self.skip_token(Separator)?;
        while self.current() == Separator {
            match self.bump() {
                Err(err @ PError::EndOfSrc) => return Err(err),
                Ok(()) => (),
                Err(_) => unreachable!(),
            }
        }
        Ok(())
    }
    fn skip_token(&mut self, tkn: TokenKind) -> PResult<()> {
        if self.current() == tkn {
            self.bump()?;
            Ok(())
        } else {
            Err(PError::NoMatch)
        }
    }
    fn current(&self) -> TokenKind {
        self.tks[self.cursor].kind
    }
}

// Combinators
impl<'src> Parser<'src> {
    fn _many<T>(&mut self, parse: Parse<'src, T>) -> Vec<T> {
        let mut out = Vec::new();
        while let Ok(n) = self.attempt(parse) {
            out.push(n);
        }
        out
    }
    fn _many1<T>(&mut self, parse: Parse<'src, T>) -> PResult<Vec<T>> {
        let mut out = Vec::new();
        out.push(self.attempt(parse)?);
        while let Ok(n) = self.attempt(parse) {
            out.push(n);
        }
        Ok(out)
    }

    fn separated<T>(&mut self, parse: Parse<'src, T>) -> PResult<Vec<T>> {
        let mut out = Vec::new();
        self.skip_separators()?;
        loop {
            match self.attempt(parse) {
                Ok(n) => out.push(n),
                Err(_) => break,
            }
            match self.skip_separators1() {
                Err(_) => break,
                Ok(_) => {}
            }
        }
        Ok(out)
    }
    fn separated1<T>(&mut self, parse: Parse<'src, T>) -> PResult<Vec<T>> {
        let mut out = Vec::new();
        self.skip_separators()?;
        out.push(parse(self)?);
        loop {
            match self.skip_separators1() {
                Err(_) => break,
                Ok(_) => {}
            }
            match self.attempt(parse) {
                Ok(n) => out.push(n),
                Err(_) => break,
            }
        }
        Ok(out)
    }

    fn _enclosed<T>(
        &mut self,
        open: Parse<'src, ()>,
        close: Parse<'src, ()>,
        parse: Parse<'src, T>,
    ) -> PResult<T> {
        open(self)?;
        let out = parse(self)?;
        close(self)?;
        Ok(out)
    }

    fn attempt<T>(&mut self, parse: Parse<'src, T>) -> PResult<T> {
        self.checkpoint();
        match parse(self) {
            ok @ Ok(_) => {
                self.finish();
                ok
            }
            err @ Err(_) => {
                self.reset();
                self.finish();
                err
            }
        }
    }

    // fn expect<T>(&mut self, parse: Parse<'src, T>) -> PResult<T> {
    //     match parse(self) {
    //         Ok(node) => Ok(node),
    //         Err(err) => {
    //             self.fatal = true;
    //             Err(err)
    //         }
    //     }
    // }

    // fn one_off<T>(&mut self, parsers: &[Parse<'src, T>]) -> PResult<T> {
    //     self.checkpoint();
    //     let mut errors = Vec::new();
    //     for parser in parsers {
    //         match parser(self) {
    //             Ok(n) => {
    //                 self.finish();
    //                 return Ok(n);
    //             }
    //             Err(err) if self.fatal => {
    //                 self.reset();
    //                 self.finish();
    //                 return Err(err);
    //             }
    //             Err(err) => {
    //                 self.reset();
    //                 errors.push(err)
    //             }
    //         }
    //     }
    //     self.reset();
    //     self.finish();
    //     return Err(PError::NoMatch);
    // }
}

#[inline]
fn token_is_operator(tkn: TokenKind) -> bool {
    match tkn {
        Plus | Minus | PlusEq | MinusEq | EqEq | LessEq | MoreEq | OpenAngle | CloseAngle
        | Asterix | Slash => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use crate::{Lexer, Parser};

    #[test]
    fn x() {
        let src = r##"
            let name = 8
        "##
        .trim()
        .chars()
        .collect::<Vec<_>>();
        let tks = Lexer::new(&src).tokenize().unwrap();
        let ast = Parser::new(tks).statement().unwrap();
        println!("{:?}", ast);
    }
}
