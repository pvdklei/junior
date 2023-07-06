#![allow(dead_code)]

use crate::lex;

use super::{utils, PError, PResult, PState, Parser, TSlice};

pub mod curry {
    use crate::{lex::TokenKind, parse::Parser};

    pub fn parse_many<N>(parser: &'static Parser<N>) -> Box<Parser<Vec<N>>> {
        Box::new(move |tks, state| super::parse_many(tks, state, parser))
    }
    pub fn parse_many1<N>(parser: &'static Parser<N>) -> Box<Parser<Vec<N>>> {
        Box::new(move |tks, state| super::parse_many1(tks, state, parser))
    }
    pub fn parse_one_of<N>(parsers: &'static [&'static Parser<N>]) -> Box<Parser<N>> {
        Box::new(move |tks, state| super::parse_one_of(tks, state, parsers))
    }
    pub fn skip_token(tkn: &'static TokenKind) -> Box<Parser<()>> {
        Box::new(move |tks, state| super::skip_token(tks, state, tkn))
    }
}

pub fn parse_many<N>(tks: &mut TSlice, state: &mut PState, parser: &Parser<N>) -> PResult<Vec<N>> {
    let mut out = Vec::new();
    while let Ok(n) = try_parse(tks, state, parser) {
        out.push(n);
    }
    Ok(out)
}

pub fn parse_many1<N>(tks: &mut TSlice, state: &mut PState, parser: &Parser<N>) -> PResult<Vec<N>> {
    let mut out = Vec::new();
    out.push(try_parse(tks, state, parser)?);
    while let Ok(n) = try_parse(tks, state, parser) {
        out.push(n);
    }
    Ok(out)
}

pub fn parse_separated<N: std::fmt::Debug>(
    tks: &mut TSlice,
    state: &mut PState,
    parser: &Parser<N>,
    sepparser: &Parser<()>,
) -> PResult<Vec<N>> {
    let mut out = Vec::new();
    utils::parse_many(tks, state, sepparser)?;
    loop {
        match try_parse(tks, state, parser) {
            Ok(n) => out.push(n),
            Err(_) => break,
        }
        match utils::parse_many1(tks, state, sepparser) {
            Err(_) => break,
            Ok(_) => {}
        }
    }
    Ok(out)
}

pub fn parse_separated1<N: std::fmt::Debug>(
    tks: &mut TSlice,
    state: &mut PState,
    parser: &Parser<N>,
    sepparser: &Parser<()>,
) -> PResult<Vec<N>> {
    let mut out = Vec::new();
    utils::parse_many(tks, state, sepparser)?;
    let first = parser(tks, state)?;
    out.push(first);

    loop {
        match utils::parse_many1(tks, state, sepparser) {
            Err(_) => break,
            Ok(_) => {}
        }
        match try_parse(tks, state, parser) {
            Ok(n) => out.push(n),
            Err(_) => break,
        }
    }
    Ok(out)
}

pub fn parse_enclosed<N>(
    tks: &mut TSlice,
    state: &mut PState,
    openparser: &Parser<()>,
    closeparser: &Parser<()>,
    parser: &Parser<N>,
) -> PResult<N> {
    openparser(tks, state)?;
    let out = parser(tks, state)?;
    closeparser(tks, state)?;
    Ok(out)
}

pub fn parse_enclosed_and_separated<N: std::fmt::Debug>(
    tks: &mut TSlice,
    state: &mut PState,
    openparser: &Parser<()>,
    closeparser: &Parser<()>,
    sepparser: &Parser<()>,
    parser: &Parser<N>,
) -> PResult<Vec<N>> {
    openparser(tks, state)?;
    let out = parse_separated(tks, state, parser, sepparser)?;
    closeparser(tks, state)?;
    Ok(out)
}

pub fn skip_many_tokens(
    tks: &mut TSlice,
    state: &mut PState,
    tkn: &'static lex::TokenKind,
) -> PResult<()> {
    while let Ok(()) = skip_token(tks, state, tkn) {}
    Ok(())
}

pub fn skip_many1_tokens(
    tks: &mut TSlice,
    state: &mut PState,
    tkn: &'static lex::TokenKind,
) -> PResult<()> {
    skip_token(tks, state, tkn)?;
    skip_many_tokens(tks, state, tkn)?;
    Ok(())
}

pub fn skip_token(tks: &mut TSlice, _state: &mut PState, tkn: &lex::TokenKind) -> PResult<()> {
    let first_token = if let Some(t) = tks.iter().next() {
        t
    } else {
        return Err(PError::EndOfSrc);
    };
    if first_token.kind == *tkn {
        tks.skip(1);
        Ok(())
    } else {
        Err(PError::NoMatch {
            expected: tkn.as_string(),
            found: first_token.kind.clone(),
        })
    }
}

impl lex::TokenKind {
    pub fn skipper(self) -> Box<Parser<()>> {
        Box::new(move |tks, state| -> Result<(), PError> { skip_token(tks, state, &self) })
    }
}

macro_rules! must_parse_if_skipped {
    ($tks:expr, $skip:expr, $parse_rest:tt) => {{
        use crate::parse::PError;
        match $skip {
            Ok(_) => match (|| $parse_rest)() {
                ok @ Ok(_) => ok,
                Err(err) => Err(PError::Expected {
                    err: Box::new(err),
                    at: $tks.current_pos(),
                }),
            },
            Err(err) => Err(err),
        }
    }};
}
pub(crate) use must_parse_if_skipped;

macro_rules! must_parse_if_next_is {
    ($tks:expr, $tkn:expr, $parse_rest:tt) => {{
        use crate::lex::Token;
        use crate::parse::PError;
        match $tks.iter().next() {
            Some(t) if t.kind == $tkn => match (|| $parse_rest)() {
                Ok(t) => Ok(t),
                Err(err) => Err(PError::Expected {
                    err: Box::new(err),
                    at: $tks.current_pos()
                }),
            },
            Some(Token { kind, .. }) => Err(PError::NoMatch {
                expected: $tkn.as_string(),
                found: kind.clone(),
            }),
            None => Err(PError::EndOfSrc),
        }
    }};
}
pub(crate) use must_parse_if_next_is;

macro_rules! parse_token_kind {
    ($tks:expr, $state:expr, $matcher:pat, $exp:expr) => {{
        use crate::parse::PError;
        let res = match $tks.iter().next() {
            Some(tkn @ lex::Token { kind: $matcher, .. }) => Ok(tkn.kind.clone()),
            Some(lex::Token { kind, .. }) => Err(PError::NoMatch {
                expected: $exp.into(),
                found: kind.clone(),
            }),
            None => Err(PError::EndOfSrc),
        };
        if let Ok(_) = res {
            $tks.skip(1);
        }
        res
    }};
}
pub(crate) use parse_token_kind;

pub fn parse_one_of<N>(tks: &mut TSlice, state: &mut PState, parsers: &[&Parser<N>]) -> PResult<N> {
    tks.checkpoint();
    let mut errors = Vec::new();
    for parser in parsers {
        match parser(tks, state) {
            Ok(n) => {
                tks.finish();
                return Ok(n);
            }
            Err(err @ PError::Expected { .. }) => {
                tks.reset();
                tks.finish();
                return Err(err);
            }
            Err(err) => {
                tks.reset();
                errors.push(err)
            }
        }
    }
    tks.reset();
    tks.finish();
    return Err(PError::OneOff(errors));
}

pub fn try_parse<N>(tks: &mut TSlice, state: &mut PState, parse: &Parser<N>) -> PResult<N> {
    tks.checkpoint();
    match parse(tks, state) {
        ok @ Ok(_) => {
            tks.finish();
            ok
        }
        err @ Err(_) => {
            tks.reset();
            tks.finish();
            err
        }
    }
}

pub fn must_parse<N>(tks: &mut TSlice, state: &mut PState, parse: &Parser<N>) -> PResult<N> {
    match parse(tks, state) {
        Ok(n) => Ok(n),
        Err(exp @ PError::Expected { .. }) => Err(exp),
        Err(err) => Err(PError::Expected {
            err: Box::new(err), 
            at: tks.current_pos()
        }),
    }
}

#[cfg(test)]
mod tests {
    use crate::lex::Separator;
    use crate::parse::{test_parser, utils::parse_many1};

    #[test]
    fn parse_many1_() {
        test_parser!(
            |tks, state| { parse_many1(tks, state, &Separator.skipper()) },
            // "let,,," => Err(_)
            "," => Ok(vec![()])
            ",,," => Ok(vec![(),(),()])
        )
    }
}
