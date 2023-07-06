use crate::{
    ast, lex,
    parse::{parse_expression, parse_pattern},
};

use super::{
    parse_identifier,
    type_::{parse_type, parse_type_enum, parse_type_struct},
    utils::{parse_one_of, parse_separated1, skip_many1_tokens, skip_many_tokens, skip_token},
    PError, PResult, PState, Parser, TSlice,
};

pub fn parse_statement(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Statement> {
    match tks.iter().next() {
        Some(lex::Token { kind, .. }) => match kind {
            lex::Let => parse_stmt_let(tks, state),
            lex::Struct => parse_stmt_struct(tks, state),
            lex::Type => parse_stmt_type(tks, state),
            lex::Enum => parse_stmt_enum(tks, state),
            _ => parse_one_of(tks, state, &[&parse_stmt_if, &parse_stmt_expr]),
        },
        None => Err(PError::EndOfSrc),
    }
}

pub fn parse_stmt_let(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Statement> {
    skip_token(tks, state, &lex::Let)?;
    let (pat, expr) = parse_assignment(tks, state, &parse_pattern)?;
    Ok(ast::StmtLet(pat, expr))
}

pub fn parse_stmt_if(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Statement> {
    skip_token(tks, state, &lex::If)?;
    let expr = parse_expression::<true>(tks, state)?;
    skip_many1_tokens(tks, state, &lex::Separator)?;
    let stmts = parse_separated1(tks, state, &parse_statement, &lex::Separator.skipper())?;
    skip_token(tks, state, &lex::End)?;
    Ok(ast::StmtIf(expr, ast::ExprDo(stmts)))
}

pub fn parse_stmt_expr(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Statement> {
    let _ = skip_token(tks, state, &lex::Mut);
    let expr = parse_expression::<true>(tks, state)?;
    Ok(ast::StmtExpr(expr))
}

pub fn parse_stmt_struct(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Statement> {
    skip_token(tks, state, &lex::Struct)?;
    let id = parse_identifier(tks, state)?;
    let ty = parse_type_struct(tks, state)?;
    Ok(ast::StmtStruct(id, ty))
}

pub fn parse_stmt_enum(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Statement> {
    skip_token(tks, state, &lex::Enum)?;
    let id = parse_identifier(tks, state)?;
    let ty = parse_type_enum(tks, state)?;
    Ok(ast::StmtEnum(id, ty))
}

pub fn parse_stmt_type(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Statement> {
    skip_token(tks, state, &lex::Type)?;
    let id = parse_identifier(tks, state)?;
    let ty = parse_type(tks, state)?;
    Ok(ast::StmtType(id, ty))
}

fn parse_assignment<L: std::fmt::Debug>(
    tks: &mut TSlice,
    state: &mut PState,
    parse_lhs: &Parser<L>,
) -> PResult<(L, ast::Expression)> {
    let lhs = parse_lhs(tks, state)?;
    skip_token(tks, state, &lex::Equals)?;
    skip_many_tokens(tks, state, &lex::Separator)?;
    let expr = parse_expression::<true>(tks, state)?;
    Ok((lhs, expr))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use crate::parse::test_parser;
    #[test]
    fn let_() {
        test_parser!(
            parse_stmt_let,
            "let   id =, 2" =>
            Ok(StmtLet(PatIdentifier("id".into()), ExprNumber("2".into())))
        )
    }
    #[test]
    fn struct_() {
        test_parser!(
            parse_stmt_struct,
            "struct A { , a: String,, b: Int ,, }" =>
            Ok(StmtStruct(
                "A".into(),
                TypeStruct(vec![
                    ("a".into(), TypeIdentifier("String".into())),
                    ("b".into(), TypeIdentifier("Int".into()))
                ])
            ))
        )
    }
}
