use crate::{
    ast, lex,
    parse::{
        parse_type,
        utils::{parse_separated1, parse_token_kind},
    },
};

use super::{
    parse_identifier, parse_modifier, parse_number, parse_operator, parse_pattern, parse_statement,
    utils::{
        curry, must_parse_if_next_is, must_parse_if_skipped, parse_enclosed_and_separated,
        parse_one_of, parse_separated, skip_many1_tokens, skip_many_tokens, skip_token, try_parse,
    },
    PResult, PState, TSlice,
};

pub fn parse_expression<const M: bool>(
    tks: &mut TSlice,
    state: &mut PState,
) -> PResult<ast::Expression> {
    let mut expr = parse_one_of(
        tks,
        state,
        &[
            &parse_expr_string,
            &parse_expr_number,
            &parse_expr_collection,
            &parse_expr_struct,
            &parse_expr_tuple,
            &parse_expr_match,
            &parse_expr_fn,
            &parse_expr_ifelse,
            &parse_expr_unop,
            &parse_expr_id,
            &parse_expr_do,
            &parse_expr_kw,
        ],
    )?;
    if M {
        if let Ok(mods) = try_parse(tks, state, &curry::parse_many1(&parse_modifier)) {
            expr = ast::ExprModified(Box::new(expr), mods);
        }
    }
    Ok(expr)
}

fn parse_expr_id(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Expression> {
    let id = parse_identifier(tks, state)?;
    Ok(ast::ExprIdentifier(id))
}

fn parse_expr_string(tks: &mut TSlice, _state: &mut PState) -> PResult<ast::Expression> {
    let enq = parse_token_kind!(tks, _state, lex::Quoted(_), "String(..)")?;
    if let lex::Quoted(s) = enq {
        Ok(ast::ExprString(s))
    } else {
        unreachable!()
    }
}

fn parse_expr_number(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Expression> {
    let n = parse_number(tks, state)?;
    Ok(ast::ExprNumber(n))
}

fn parse_expr_collection(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Expression> {
    must_parse_if_next_is!(tks, lex::OpenBracket, {
        let exprs = parse_enclosed_and_separated(
            tks,
            state,
            &lex::OpenBracket.skipper(),
            &lex::CloseBracket.skipper(),
            &lex::Separator.skipper(),
            &parse_expression::<true>,
        )?;
        Ok(ast::ExprCollection(exprs))
    })
}

fn parse_expr_tuple(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Expression> {
    must_parse_if_next_is!(tks, lex::OpenParen, {
        let exprs = parse_enclosed_and_separated(
            tks,
            state,
            &lex::OpenParen.skipper(),
            &lex::CloseParen.skipper(),
            &lex::Separator.skipper(),
            &parse_expression::<true>,
        )?;
        Ok(ast::ExprTuple(exprs))
    })
}

fn parse_expr_struct(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Expression> {
    must_parse_if_next_is!(tks, lex::OpenBrace, {
        let exprs = parse_enclosed_and_separated(
            tks,
            state,
            &lex::OpenBrace.skipper(),
            &lex::CloseBrace.skipper(),
            &lex::Separator.skipper(),
            &|tks, state| {
                let id = parse_identifier(tks, state)?;
                if let Ok(()) = skip_token(tks, state, &lex::Equals) {
                    let expr = parse_expression::<true>(tks, state)?;
                    Ok((id, Some(expr)))
                } else {
                    Ok((id, None))
                }
            },
        )?;
        Ok(ast::ExprStruct(exprs))
    })
}

fn parse_expr_fn(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Expression> {
    must_parse_if_skipped!(tks, skip_token(tks, state, &lex::Fn), {
        let pat = if let Ok(pat) = parse_pattern(tks, state) {
            pat
        } else {
            ast::PatTuple(Vec::new())
        };
        let retty = if skip_token(tks, state, &lex::Minus).is_ok()
            && skip_token(tks, state, &lex::CloseAngle).is_ok()
        {
            Some(parse_type(tks, state)?)
        } else {
            None
        };
        skip_many_tokens(tks, state, &lex::Separator)?;
        let stmts = parse_separated1(tks, state, &parse_statement, &lex::Separator.skipper())?;
        skip_many_tokens(tks, state, &lex::Separator)?;
        skip_token(tks, state, &lex::End)?;
        Ok(ast::ExprFn(pat, retty, Box::new(ast::ExprDo(stmts))))
    })
}

fn parse_expr_do(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Expression> {
    must_parse_if_skipped!(tks, skip_token(tks, state, &lex::Do), {
        let stmts = parse_separated(tks, state, &parse_statement, &lex::Separator.skipper())?;
        skip_token(tks, state, &lex::End)?;
        Ok(ast::ExprDo(stmts))
    })
}

fn parse_expr_ifelse(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Expression> {
    must_parse_if_skipped!(tks, skip_token(tks, state, &lex::If), {
        let cond = parse_expression::<true>(tks, state)?;
        skip_many1_tokens(tks, state, &lex::Separator)?;
        let then = parse_separated1(tks, state, &parse_statement, &lex::Separator.skipper())?;
        skip_token(tks, state, &lex::Else)?;
        let else_ = parse_separated1(tks, state, &parse_statement, &lex::Separator.skipper())?;
        skip_token(tks, state, &lex::End)?;
        Ok(ast::ExprIfElse(Box::new((
            cond,
            ast::ExprDo(then),
            ast::ExprDo(else_),
        ))))
    })
}

fn parse_expr_match(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Expression> {
    must_parse_if_skipped!(tks, skip_token(tks, state, &lex::Match), {
        let expr = parse_expression::<true>(tks, state)?;
        let match_as = match try_parse(tks, state, &lex::As.skipper()) {
            Ok(_) => Some(parse_pattern(tks, state)?),
            Err(_) => None,
        };
        skip_many1_tokens(tks, state, &lex::Separator)?;
        let arms = parse_separated1(tks, state, &parse_match_arm, &lex::Separator.skipper())?;
        skip_token(tks, state, &lex::End)?;
        Ok(ast::ExprMatch(Box::new(expr), match_as, arms))
    })
}

fn parse_match_arm(
    tks: &mut TSlice,
    state: &mut PState,
) -> PResult<(ast::Pattern, ast::Expression)> {
    let pat = parse_pattern(tks, state)?;
    skip_token(tks, state, &lex::Then)?;
    if let Ok(()) = skip_many1_tokens(tks, state, &lex::Separator) {
        let stmts = parse_separated1(tks, state, &parse_statement, &lex::Separator.skipper())?;
        skip_token(tks, state, &lex::End)?;
        Ok((pat, ast::ExprDo(stmts)))
    } else {
        let expr = parse_expression::<true>(tks, state)?;
        Ok((pat, expr))
    }
}

fn parse_expr_unop(tks: &mut TSlice, state: &mut PState) -> PResult<ast::Expression> {
    let op = parse_operator(tks, state)?;
    let expr = parse_expression::<true>(tks, state)?;
    Ok(ast::ExprUnaryOperation(op, Box::new(expr)))
}

#[allow(unused_parens)]
fn parse_expr_kw(tks: &mut TSlice, _state: &mut PState) -> PResult<ast::Expression> {
    use lex::*;
    let kw = parse_token_kind!(
        tks,
        _state,
        (CapitalSelf
            | SmallSelf
            | Mod
            | Crate
            | Var
            | Ref
            | Val
            | Copy
            | Clone
            | Into
            | Jr
            | Rs
            | True
            | False
            | Vec_
            | List
            | Str
            | String_),
        "Keyword(..)"
    )?;
    Ok(ast::ExprKewword(kw))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
    use crate::parse::test_parser;

    #[test]
    fn ifelse() {
        test_parser!(
            parse_expr_ifelse,
            "if a, b else c end",
            "if a,, b,, , else,  , c, , end" =>
            Ok(ExprIfElse(Box::new((
                ExprIdentifier("a".into()),
                ExprIdentifier("b".into()).expr_do(),
                ExprIdentifier("c".into()).expr_do()
            ))))
        )
    }

    #[test]
    fn do_() {
        test_parser!(
            parse_expr_do,
            "do let b = 2, let a = b,, b = 2, a,,, end",
            "do,,, let b = 2,,, let a = b, , b = 2, a end" =>
            Ok(ExprDo(vec![
                StmtLet(PatIdentifier("b".into()), ExprNumber("2".into())),
                StmtLet(PatIdentifier("a".into()), ExprIdentifier("b".into())),
                StmtExpr(
                    ExprModified(
                        Box::new(ExprIdentifier("b".into())),
                        vec![ModBinaryOperation(vec![lex::Equals], ExprNumber("2".into()))]
                    )
                ),
                StmtExpr(ExprIdentifier("a".into()))
            ]))
        )
    }
}
