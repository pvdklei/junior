use crate::tks::{
    Token,
    TokenKind::{self, *},
    Tokens,
};
use std::ops::Fn;

pub struct Lexer<'src> {
    src: &'src [char],
    cursor: usize,
    current: char,
    tks: Tokens<'src>,
}

type LResult<T = ()> = Result<T, LError>;
#[derive(Debug, Clone)]
pub enum LError {
    EndOfSrc,
    NoMatch,
}

impl<'src> Lexer<'src> {
    pub fn new(src: &'src [char]) -> Self {
        Self {
            src,
            cursor: 0,
            current: src[0],
            tks: Tokens::new(src),
        }
    }
    pub fn tokenize(mut self) -> LResult<Tokens<'src>> {
        // TODO: waarom?
        self.tks.push(Token {
            kind: Separator,
            len: 0,
        });

        loop {
            match self.token() {
                Ok(()) => continue,
                Err(_) => break,
            }
        }
        Ok(self.tks)
    }
    fn token(&mut self) -> LResult {
        match &self.src[self.cursor..] {
            ['+', '=', ..] => self.lex(2, PlusEq),
            ['+', ..] => self.lex(1, Plus),
            ['-', '>', ..] => self.lex(2, Rarrow),
            ['-', '=', ..] => self.lex(2, MinusEq),
            ['-', ..] => self.lex(1, Minus),
            ['*', '=', ..] => self.lex(2, MultEq),
            ['*', ..] => self.lex(1, Asterix),
            ['/', '/', ..] => self.comment(),
            ['/', ..] => self.lex(1, Slash),
            ['&', ..] => self.lex(1, And),
            ['|', ..] => self.lex(1, Or),
            ['=', '=', ..] => self.lex(1, EqEq),
            ['=', ..] => self.lex(1, Equals),
            ['<', ..] => self.lex(1, OpenAngle),
            ['>', ..] => self.lex(1, CloseAngle),
            ['(', ..] => self.lex(1, OpenParen),
            [')', ..] => self.lex(1, CloseParen),
            ['{', ..] => self.lex(1, OpenBrace),
            ['}', ..] => self.lex(1, CloseBrace),
            ['[', ..] => self.lex(1, OpenBracket),
            [']', ..] => self.lex(1, CloseBracket),
            ['?', ..] => self.lex(1, Question),
            ['%', ..] => self.lex(1, Percentage),
            ['.', '.', ..] => self.lex(2, DotDot),
            ['.', ..] => self.lex(1, Dot),
            [':', ..] => self.lex(1, Colon),
            [',', ..] | ['\n', ..] => {
                if self.last_token_was_sep() {
                    self.add_to_last_token();
                    self.skip()
                } else {
                    self.lex(1, Separator)
                }
            }
            ['_', ..] => self.identifier(),
            ['\"', ..] => self.quoted(),
            [' ', ..] | ['\t', ..] => {
                self.skip()?;
                self.add_to_last_token();
                Ok(())
            }
            [c, ..] if is_alpha(*c) => self.identifier_or_keyword(),
            [c, ..] if is_number(*c) => self.number(),
            _ => Err(LError::NoMatch),
        }
    }
    fn comment(&mut self) -> LResult {
        while self.current() != '\n' {
            self.skip()?;
            self.add_to_last_token();
        }
        self.skip()?;
        self.add_to_last_token();
        Ok(())
    }
    fn identifier(&mut self) -> LResult {
        let start = self.cursor;
        self.skip_while(in_identifier)?;
        self.tks.push(Token {
            kind: Identifier,
            len: (self.cursor - start) as u16,
        });
        Ok(())
    }
    fn identifier_or_keyword(&mut self) -> LResult {
        let start = self.cursor;
        self.skip_while(is_alpha)?;
        if let Some(kind) = Self::keyword(&self.src[start..self.cursor]) {
            self.tks.push(Token {
                kind,
                len: (self.cursor - start) as u16,
            });
            return Ok(());
        }
        self.skip_while(in_identifier)?;
        self.tks.push(Token {
            kind: Identifier,
            len: (self.cursor - start) as u16,
        });
        Ok(())
    }
    fn quoted(&mut self) -> LResult {
        let start = self.cursor;
        self.skip()?; // opening "
        while self.current() != '"' {
            self.skip()?
        }
        self.skip()?; // closing "
        self.tks.push(Token {
            kind: Quoted,
            len: (self.cursor - start) as u16,
        });
        Ok(())
    }
    fn number(&mut self) -> LResult {
        let start = self.cursor;
        self.skip_while(is_number)?;
        if self.current() == '.' {
            self.skip()?;
            self.skip_while(is_number)?;
        }
        self.tks.push(Token {
            kind: Number,
            len: (self.cursor - start) as u16,
        });
        Ok(())
    }
    fn keyword(chars: &'src [char]) -> Option<TokenKind> {
        match chars.len() {
            2 => match chars {
                ['i', 'f'] => Some(If),
                ['f', 'n'] => Some(Fn),
                ['d', 'o'] => Some(Do),
                ['a', 's'] => Some(As),
                ['j', 'r'] => Some(Jr),
                ['r', 's'] => Some(Rs),
                ['o', 'k'] => Some(Ok_),
                _ => None,
            },
            3 => match chars {
                ['l', 'e', 't'] => Some(Let),
                ['m', 'u', 't'] => Some(Mut),
                ['e', 'n', 'd'] => Some(End),
                ['s', 't', 'r'] => Some(Str),
                ['r', 'e', 'f'] => Some(Ref),
                ['v', 'a', 'l'] => Some(Val),
                ['v', 'a', 'r'] => Some(Var),
                ['b', 'o', 'x'] => Some(Box_),
                ['v', 'e', 'c'] => Some(Vec_),
                ['e', 'r', 'r'] => Some(Err_),
                ['i', 'n', 't'] => Some(Int),
                _ => None,
            },
            4 => match chars {
                ['s', 'e', 'l', 'f'] => Some(SmallSelf),
                ['l', 'o', 'o', 'p'] => Some(Loop),
                ['S', 'e', 'l', 'f'] => Some(CapitalSelf),
                ['e', 'l', 's', 'e'] => Some(Else),
                ['e', 'n', 'u', 'm'] => Some(Enum),
                ['t', 'y', 'p', 'e'] => Some(Type),
                ['i', 't', 'e', 'r'] => Some(Iter),
                ['t', 'h', 'e', 'n'] => Some(Then),
                ['l', 'i', 's', 't'] => Some(List),
                ['t', 'r', 'u', 'e'] => Some(True),
                ['n', 'o', 'n', 'e'] => Some(None_),
                ['s', 'o', 'm', 'e'] => Some(Some_),
                ['b', 'o', 'o', 'l'] => Some(Bool),
                _ => None,
            },
            5 => match chars {
                ['m', 'a', 't', 'c', 'h'] => Some(Match),
                ['b', 'r', 'e', 'a', 'k'] => Some(Break),
                ['c', 'o', 'n', 's', 't'] => Some(Const),
                ['t', 'r', 'a', 'i', 't'] => Some(Trait),
                ['u', 'n', 'i', 'o', 'n'] => Some(Union),
                ['f', 'a', 'l', 's', 'e'] => Some(False),
                ['f', 'l', 'o', 'a', 't'] => Some(Float),
                _ => None,
            },
            6 => match chars {
                ['s', 't', 'r', 'u', 'c', 't'] => Some(Struct),
                ['r', 'e', 's', 'u', 'l', 't'] => Some(Result_),
                ['o', 'p', 't', 'i', 'o', 'n'] => Some(Option_),
                ['s', 't', 'r', 'i', 'n', 'g'] => Some(String_),
                _ => None,
            },
            8 => match chars {
                ['c', 'o', 'n', 't', 'i', 'n', 'u', 'e'] => Some(Continue),
                _ => None,
            },
            _ => None,
        }
    }
}

impl<'src> Lexer<'src> {
    const EOF: char = '~';
    fn lex(&mut self, len: u16, tknkind: TokenKind) -> LResult {
        self.cursor += len as usize;
        self.tks.push(Token { kind: tknkind, len });
        Ok(())
    }
    fn skip(&mut self) -> LResult {
        self.cursor += 1;
        if self.cursor >= self.src.len() {
            self.current = Self::EOF;
            Err(LError::EndOfSrc)
        } else {
            self.current = self.src[self.cursor];
            Ok(())
        }
    }
    fn skip_while(&mut self, f: impl Fn(char) -> bool) -> LResult {
        loop {
            if f(self.current()) {
                let _ = self.skip();
            } else {
                return Ok(());
            }
        }
    }
    fn current(&self) -> char {
        self.current
    }
    fn add_to_last_token(&mut self) {
        let idx_last = self.tks.len() - 1;
        self.tks[idx_last].len += 1
    }
    fn last_token_was_sep(&self) -> bool {
        match self.tks.last() {
            Some(Token { kind, .. }) if *kind == Separator => true,
            _ => false,
        }
    }
}

fn in_identifier(c: char) -> bool {
    c.is_alphabetic() || c == '_' || c.is_numeric()
}

fn is_number(c: char) -> bool {
    c.is_numeric()
}

fn is_alpha(c: char) -> bool {
    c.is_alphabetic()
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_quotes() {
        let src: Vec<_> = r#"
            // a comment

            let hai: String = "haiii"


            let an_fn = fn
                let b = "yooo"
                let c = 222.2
            end


        "#
        .trim()
        .chars()
        .collect();
        let tks = Lexer::new(&src).tokenize().unwrap();
        println!("{:?}", tks);
    }
}
