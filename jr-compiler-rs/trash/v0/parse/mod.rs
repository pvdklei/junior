pub mod expr;
pub mod modifier;
pub mod pattern;
pub mod stmt;
pub mod type_;
pub mod utils;

pub use expr::parse_expression;
pub use modifier::parse_modifier;
pub use pattern::parse_pattern;
pub use stmt::parse_statement;
pub use type_::parse_type;

use crate::{
    ast, lex,
    parse::utils::{parse_token_kind, skip_token},
};
use std::ops::Deref;

use self::utils::{parse_many1, skip_many_tokens};

pub struct PState {
    pub view: (usize, usize),
}

impl Default for PState {
    fn default() -> Self {
        PState { view: (0, 0) }
    }
}

impl PState {
    pub fn stretch(&mut self, n: usize) {
        self.view = (self.view.0, self.view.1 + n)
    }
    pub fn shorten(&mut self, n: usize) {
        self.view = (self.view.0 + n, self.view.1)
    }
}

pub type Parser<N> = dyn Fn(&mut TSlice, &mut PState) -> PResult<N>;
pub trait ParseFn<N>: Fn(&mut TSlice, &mut PState) -> PResult<N> {}
impl<N, T> ParseFn<N> for T where T: Fn(&mut TSlice, &mut PState) -> PResult<N> {}

pub type PResult<N> = Result<N, PError>;

#[derive(Debug, PartialEq, Clone)]
pub enum PError {
    EndOfSrc,
    NoMatch {
        expected: String,
        found: lex::TokenKind,
    },
    OneOff(Vec<PError>),
    Expected {
        err: Box<PError>,
        at: (usize, usize)
    },
}

#[derive(Debug)]
pub struct TSlice<'a> {
    pub tks: &'a [lex::Token],
    pub history: Vec<&'a [lex::Token]>,
}

impl<'a> From<&'a [lex::Token]> for TSlice<'a> {
    fn from(tks: &'a [lex::Token]) -> Self {
        Self {
            tks,
            history: Vec::new(),
        }
    }
}

impl<'a> Deref for TSlice<'a> {
    type Target = [lex::Token];
    fn deref(&self) -> &Self::Target {
        self.tks
    }
}

impl<'a> TSlice<'a> {
    fn checkpoint(&mut self) {
        self.history.push(self.tks);
    }
    fn reset(&mut self) {
        self.tks = self.history.last().unwrap();
    }
    fn finish(&mut self) {
        self.history.pop();
    }
    fn skip(&mut self, n: usize) {
        self.tks = &self.tks[n..];
    }
    fn current_pos(&self) -> (usize, usize) {
        self.tks[0].from
    }
}

pub fn parse_file(tks: &mut TSlice, state: &mut PState) -> PResult<Vec<ast::Statement>> {
    let mut out = Vec::new();
    skip_many_tokens(tks, state, &lex::Separator)?;
    loop {
        match parse_statement(tks, state) {
            Ok(stmt) => out.push(stmt),
            Err(PError::EndOfSrc) => break,
            Err(err) => return Err(err),
        }
        skip_many_tokens(tks, state, &lex::Separator)?;
    }
    Ok(out)
}

fn parse_identifier(tks: &mut TSlice, _state: &mut PState) -> PResult<ast::Identifier> {
    let ok = match tks.iter().next() {
        Some(t) => {
            if let lex::Id(s) = &t.kind {
                Ok(s.clone())
            } else {
                return Err(PError::NoMatch {
                    expected: "Id(..)".into(),
                    found: t.kind.clone(),
                });
            }
        }
        None => return Err(PError::EndOfSrc),
    };
    tks.skip(1);
    ok
}

fn parse_number(tks: &mut TSlice, state: &mut PState) -> PResult<String> {
    let before = parse_token_kind!(tks, state, lex::Number(_), "Number(..)");
    let dot = skip_token(tks, state, &lex::Dot);
    match (before, dot) {
        (Ok(lex::Number(before)), Ok(_)) => {
            let after = if let lex::Number(n) =
                parse_token_kind!(tks, state, lex::Number(_), "Number(..)")?
            {
                n
            } else {
                unreachable!()
            };
            Ok(format!("{}.{}", before, after))
        }
        (Ok(lex::Number(n)), Err(_)) => Ok(n.to_string()),
        (Err(_), Ok(_)) => {
            let after = if let lex::Number(n) =
                parse_token_kind!(tks, state, lex::Number(_), "Number(..)")?
            {
                n
            } else {
                unreachable!()
            };
            Ok(format!(".{}", after))
        }
        (Err(err), _) => Err(err),
        _ => unreachable!(),
    }
}

#[allow(unused_parens)]
fn parse_operator(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Operator> {
    let tks = parse_many1(tks, state, &|tks: &mut TSlice, _state| {
        parse_token_kind!(
            tks,
            state,
            (lex::CloseAngle
                | lex::OpenAngle
                | lex::Plus
                | lex::Minus
                | lex::Asterix
                | lex::Slash
                | lex::Equals),
            "Operator(..)"
        )
    })?;
    Ok(tks)
}

#[cfg(test)]
macro_rules! test_parser {
    ($parser:expr, $( $( $src:expr ),* => $tree:expr )* ) => {{
        use crate::parse::lex_and_parse_with;
        $( $( assert_eq!(lex_and_parse_with($src, &$parser), $tree); )* )*
    }};
}

#[cfg(test)]
pub(crate) use test_parser;

pub fn lex_and_parse_with<N>(src: &str, parser: &Parser<N>) -> PResult<N> {
    use crate::{lex::map_kind, tokenize};
    let tks = tokenize(src).unwrap();
    println!("Tokens: {:?}", map_kind(&tks));
    let mut tks = tks.deref().into();
    let n = parser(&mut tks, &mut Default::default());
    n
}
