use crate::{ast, lex};

use super::{
    parse_expression, parse_operator, parse_type,
    utils::{parse_enclosed_and_separated, parse_one_of, skip_many_tokens, skip_token},
    PResult, PState, TSlice,
};

pub fn parse_modifier(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Modifier> {
    parse_one_of(
        tks,
        state,
        &[
            &parse_mod_attribute,
            &parse_mod_question,
            &parse_mod_call,
            &parse_mod_binop,
        ],
    )
}

fn parse_mod_binop(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Modifier> {
    let op = parse_operator(tks, state)?;
    let expr = parse_expression::<true>(tks, state)?;
    Ok(ast::ModBinaryOperation(op, expr))
}

fn parse_mod_attribute(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Modifier> {
    skip_many_tokens(tks, state, &lex::Separator)?;
    skip_token(tks, state, &lex::Dot)?;
    if let Ok(n) = parse_mod_generics(tks, state) {
        Ok(n)
    } else {
        let expr = parse_expression::<false>(tks, state)?;
        Ok(ast::ModAttribute(expr))
    }
}

fn parse_mod_call(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Modifier> {
    Ok(ast::ModCall(parse_expression::<false>(tks, state)?))
}

fn parse_mod_question(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Modifier> {
    skip_token(tks, state, &lex::Question)?;
    Ok(ast::ModQuestion)
}

fn parse_mod_generics(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Modifier> {
    let tys = parse_enclosed_and_separated(
        tks,
        state,
        &lex::OpenAngle.skipper(),
        &lex::CloseAngle.skipper(),
        &lex::Separator.skipper(),
        &parse_type,
    )?;
    Ok(ast::ModGenerics(tys))
}
