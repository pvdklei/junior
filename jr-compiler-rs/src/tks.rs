use std::{
    fmt::Debug,
    ops::{Deref, DerefMut},
};

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub len: u16,
}

pub use TokenKind::*;
#[derive(Clone, Copy, Debug, PartialEq, Hash)]
#[repr(u16)]
pub enum TokenKind {
    // symbols
    Colon,
    Dot,
    Asterix,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    OpenAngle,
    CloseAngle,
    Plus,
    Minus,
    And,
    Or,
    HashTag,
    Slash,
    Percentage,
    Question,
    Equals,
    Separator,
    Underscore,
    LessEq,
    MoreEq,
    PlusEq,
    MinusEq,
    MultEq,
    DivEq,
    EqEq,
    Rarrow,
    DotDot,

    // kw control flow
    If,
    Else,
    Loop,
    Break,
    Continue,
    Match,
    SmallSelf,
    CapitalSelf,
    Const,
    Fn,
    Do,
    End,
    Then,
    As,

    // kw statement
    Type,
    Union,
    Enum,
    Trait,
    Struct,
    Let,
    Mut,

    // wk types
    Ref,
    Var,
    Val,
    String_,
    Str,
    Box_,
    Vec_,
    List,
    Some_,
    None_,
    Option_,
    Result_,
    Err_,
    Ok_,
    Bool,
    True,
    False,
    Iter,

    // other
    Copy,
    Clone,
    Move,
    Moved,
    Weak,
    Mod,
    Crate,
    Into,
    Jr,
    Rs,
    Float,
    Int,

    // compound
    Identifier,
    Quoted,
    Number,
}

pub struct Tokens<'src> {
    tks: Vec<Token>,
    pub src: &'src [char],
}

impl<'src> Deref for Tokens<'src> {
    type Target = Vec<Token>;
    fn deref(&self) -> &Self::Target {
        &self.tks
    }
}

impl<'src> DerefMut for Tokens<'src> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.tks
    }
}

#[derive(Debug)]
struct Id(String);
#[derive(Debug)]
struct Num(String);
#[derive(Debug)]
struct Str(String);

impl<'src> std::fmt::Debug for Tokens<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut lst = f.debug_list();
        let mut idx: usize = 0;
        for Token { kind, len } in &self.tks {
            match kind {
                Identifier | Quoted | Number => {
                    let mut entry = self.src[idx..idx + *len as usize]
                        .iter()
                        .take_while(|c| !c.is_whitespace())
                        .collect::<String>();
                    match kind {
                        Identifier => lst.entry(&Id(entry)),
                        Quoted => {
                            // first remove quotes
                            entry = entry.split_off(1);
                            let len = entry.len();
                            entry.truncate(len - 2);
                            lst.entry(&Str(entry))
                        }
                        Number => lst.entry(&Num(entry)),
                        _ => unreachable!(),
                    };
                }
                tkn => {
                    lst.entry(tkn);
                }
            }
            idx += *len as usize;
        }
        lst.finish()
    }
}

impl<'src> Tokens<'src> {
    pub fn new(src: &'src [char]) -> Self {
        Self {
            src,
            tks: Vec::new(),
        }
    }
}
