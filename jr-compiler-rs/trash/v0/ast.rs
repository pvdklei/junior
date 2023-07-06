pub use Expression::*;
pub use Modifier::*;
pub use Pattern::*;
pub use Statement::*;
pub use Type::*;

use crate::lex;

pub type Ast = Vec<Statement>;

#[derive(Debug, PartialEq, Clone)]
pub enum Statement {
    StmtLet(Pattern, Expression),
    StmtStruct(Identifier, Type),
    StmtEnum(Identifier, Type),
    StmtType(Identifier, Type),
    StmtExpr(Expression),
    StmtIf(Expression, Expression),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Expression {
    ExprModified(Box<Expression>, Vec<Modifier>),
    ExprIfElse(Box<(Expression, Expression, Expression)>),
    ExprMatch(Box<Expression>, Option<Pattern>, Vec<(Pattern, Expression)>),
    ExprFn(Pattern, Option<Type>, Box<Expression>),
    ExprDo(Vec<Statement>),
    ExprNumber(Number),
    ExprString(String),
    ExprCollection(Vec<Expression>),
    ExprTuple(Vec<Expression>),
    ExprStruct(Vec<(Identifier, Option<Expression>)>),
    ExprUnaryOperation(Operator, Box<Expression>),
    ExprIdentifier(Identifier),
    ExprKewword(lex::TokenKind),
}

impl Expression {
    pub fn expr_do(self) -> Self {
        ExprDo(vec![StmtExpr(self)])
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    PatStruct(Vec<Pattern>),
    PatTuple(Vec<Pattern>),
    PatNumber(Number),
    PatString(String),
    PatCollection(Vec<Pattern>),
    PatWildcard,
    PatDotDot,
    PatLabeled(Identifier, Box<Pattern>),
    PatIdentifier(Identifier),
    PatValued(Identifier, Box<Pattern>),
    PatTyped(Box<Pattern>, Type),
    PatGuarded(Box<Pattern>, Box<Expression>),
    PatOr(Box<(Pattern, Pattern)>),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    TypeEnum(Vec<(Identifier, Type)>),
    TypeStruct(Vec<(Identifier, Type)>),
    TypeTuple(Vec<Type>),
    TypeCollection(Box<Type>),
    TypeIdentifier(Identifier),
    TypeWithPathSegment(Identifier, Box<Type>),
    TypeWithGenerics(Box<Type>, Vec<Type>),
    TypeKewword(lex::TokenKind),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Modifier {
    ModAttribute(Expression),
    ModKeywordAttribute(lex::TokenKind),
    ModCall(Expression),
    ModQuestion,
    ModBinaryOperation(Operator, Expression),
    ModGenerics(Vec<Type>),
}

pub type Operator = Vec<lex::TokenKind>;

pub type Identifier = String;

pub type Number = String;
