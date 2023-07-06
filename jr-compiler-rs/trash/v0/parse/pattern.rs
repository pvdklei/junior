use crate::{ast, lex, parse::utils::parse_token_kind};

use super::{
    parse_expression, parse_identifier, parse_number, parse_type,
    utils::{curry, parse_enclosed_and_separated, parse_one_of, skip_token, try_parse},
    PResult, PState, TSlice,
};

pub fn parse_pattern(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Pattern> {
    let pat = parse_one_of(
        tks,
        state,
        &[
            &parse_pat_string,
            &parse_pat_number,
            &parse_pat_collection,
            &parse_pat_struct,
            &parse_pat_tuple,
            &parse_pat_label_or_id,
            &parse_pat_wildcard,
            &parse_pat_dotdot,
        ],
    )?;
    if let Ok(()) = try_parse(tks, state, &lex::Colon.skipper()) {
        let ty = parse_type(tks, state)?;
        return Ok(ast::PatTyped(Box::new(pat), ty));
    }
    if let Ok(()) = try_parse(tks, state, &lex::Or.skipper()) {
        let pat2 = parse_pattern(tks, state)?;
        return Ok(ast::PatOr(Box::new((pat, pat2))));
    }
    if let Ok(()) = try_parse(tks, state, &lex::If.skipper()) {
        let cond = parse_expression::<true>(tks, state)?;
        return Ok(ast::PatGuarded(Box::new(pat), Box::new(cond)));
    }
    Ok(pat)
}

fn parse_pat_string(tks: &mut TSlice, _state: &mut PState) -> PResult<ast::Pattern> {
    let enq = parse_token_kind!(tks, _state, lex::Quoted(_), "String(..)")?;
    if let lex::Quoted(s) = enq {
        Ok(ast::PatString(s))
    } else {
        unreachable!()
    }
}

fn parse_pat_number(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Pattern> {
    let n = parse_number(tks, state)?;
    Ok(ast::PatNumber(n))
}

fn parse_pat_collection(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Pattern> {
    let pats = parse_enclosed_and_separated(
        tks,
        state,
        &lex::OpenBracket.skipper(),
        &lex::CloseBracket.skipper(),
        &lex::Separator.skipper(),
        &parse_pattern,
    )?;
    Ok(ast::PatCollection(pats))
}

fn parse_pat_tuple(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Pattern> {
    let pats = parse_enclosed_and_separated(
        tks,
        state,
        &lex::OpenParen.skipper(),
        &lex::CloseParen.skipper(),
        &lex::Separator.skipper(),
        &parse_pattern,
    )?;
    Ok(ast::PatTuple(pats))
}

fn parse_pat_struct(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Pattern> {
    let pats = parse_enclosed_and_separated(
        tks,
        state,
        &lex::OpenBrace.skipper(),
        &lex::CloseBrace.skipper(),
        &lex::Separator.skipper(),
        &curry::parse_one_of(&[&parse_pat_struct_element, &parse_pat_dotdot]),
    )?;
    Ok(ast::PatStruct(pats))
}

fn parse_pat_struct_element(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Pattern> {
    let id = parse_identifier(tks, state)?;
    if let Ok(()) = try_parse(tks, state, &lex::Equals.skipper()) {
        let value = parse_pattern(tks, state)?;
        return Ok(ast::PatValued(id, Box::new(value)));
    }
    Ok(ast::PatIdentifier(id))
}

fn parse_pat_dotdot(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Pattern> {
    skip_token(tks, state, &lex::Dot)?;
    skip_token(tks, state, &lex::Dot)?;
    Ok(ast::PatDotDot)
}

fn parse_pat_wildcard(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Pattern> {
    skip_token(tks, state, &lex::Underscore)?;
    Ok(ast::PatWildcard)
}

fn parse_pat_label_or_id(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Pattern> {
    let id = parse_identifier(tks, state)?;
    if let Ok(pat) = try_parse(tks, state, &parse_pattern) {
        return Ok(ast::PatLabeled(id, Box::new(pat)));
    }
    Ok(ast::PatIdentifier(id))
}
