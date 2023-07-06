use crate::{ast, lex};

use super::{
    parse_identifier,
    utils::{parse_enclosed_and_separated, parse_one_of, parse_token_kind, skip_token, try_parse},
    PResult, PState, TSlice,
};

pub fn parse_type(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Type> {
    let ty = parse_one_of(
        tks,
        state,
        &[
            &parse_type_id_or_path,
            &parse_type_struct,
            &parse_type_enum,
            &parse_type_collection,
            &parse_type_tuple,
            &parse_type_kw,
        ],
    )?;
    if let Ok(g) = try_parse(tks, state, &parse_type_generics) {
        Ok(ast::TypeWithGenerics(Box::new(ty), g))
    } else {
        Ok(ty)
    }
}

pub fn parse_type_struct(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Type> {
    let stmts = parse_enclosed_and_separated(
        tks,
        state,
        &lex::OpenBrace.skipper(),
        &lex::CloseBrace.skipper(),
        &lex::Separator.skipper(),
        &|tks, state| {
            let id: ast::Identifier = parse_identifier(tks, state)?;
            skip_token(tks, state, &lex::Colon)?;
            let ty = parse_type(tks, state)?;
            Ok((id, ty))
        },
    )?;
    Ok(ast::TypeStruct(stmts))
}

pub fn parse_type_enum(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Type> {
    let options = parse_enclosed_and_separated(
        tks,
        state,
        &lex::OpenBrace.skipper(),
        &lex::CloseBrace.skipper(),
        &lex::Separator.skipper(),
        &|tks, state| {
            let id: ast::Identifier = parse_identifier(tks, state)?;
            if let Ok(ty) = parse_type(tks, state) {
                Ok((id, ty))
            } else {
                Ok((id, ast::TypeTuple(Vec::new())))
            }
        },
    )?;
    Ok(ast::TypeEnum(options))
}

pub fn parse_type_tuple(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Type> {
    let options = parse_enclosed_and_separated(
        tks,
        state,
        &lex::OpenParen.skipper(),
        &lex::CloseParen.skipper(),
        &lex::Separator.skipper(),
        &parse_type,
    )?;
    Ok(ast::TypeTuple(options))
}

pub fn parse_type_collection(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Type> {
    skip_token(tks, state, &lex::OpenBracket)?;
    let ty = parse_type(tks, state)?;
    skip_token(tks, state, &lex::CloseBracket)?;
    Ok(ast::TypeCollection(Box::new(ty)))
}

pub fn parse_type_id_or_path(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Type> {
    let id = parse_identifier(tks, state)?;
    if let Ok(ty) = try_parse(tks, state, &parse_type) {
        Ok(ast::TypeWithPathSegment(id, Box::new(ty)))
    } else {
        Ok(ast::TypeIdentifier(id))
    }
}

pub fn parse_type_generics(tks: &mut TSlice, state: &mut PState) -> PResult<Vec<ast::Type>> {
    parse_enclosed_and_separated(
        tks,
        state,
        &lex::OpenAngle.skipper(),
        &lex::CloseAngle.skipper(),
        &lex::Separator.skipper(),
        &parse_type,
    )
}

#[allow(unused_parens)]
pub fn parse_type_kw(tks: &mut TSlice, _state: &mut PState) -> PResult<ast::Type> {
    use lex::*;
    Ok(ast::TypeKewword(parse_token_kind!(
        tks,
        _state,
        (Ref | Val
            | Var
            | Enum
            | Struct
            | Union
            | Box_
            | Vec_
            | List
            | Bool
            | Float
            | Int
            | String_
            | Str),
        "Kewword(..)"
    )?))
}
