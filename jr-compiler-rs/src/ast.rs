pub use Expression::*;
pub use Modifier::*;
pub use Pattern::*;
pub use Statement::*;
pub use Type::*;

use crate::{tks::TokenKind, meta};

pub type Ast = Vec<Statement>;

#[derive(Debug, PartialEq, Clone, Hash)]
pub enum Statement {
    StmtLet(Pattern, Expression),
    StmtStruct(Identifier, Type),
    StmtEnum(Identifier, Type),
    StmtType(Identifier, Type),
    StmtExpr(Expression),
    StmtIf(Expression, Expression),
}

#[derive(Debug, PartialEq, Clone, Hash)]
pub enum Expression {
    ExprModified(Box<Self>, Vec<Modifier>),
    ExprIfElse(Box<(Self, Self, Self)>),
    ExprMatch(Box<Self>, Option<Pattern>, Vec<(Pattern, Self)>),
    ExprFn(Pattern, Option<Type>, Box<Self>),
    ExprDo(Vec<Statement>),
    ExprNumber(Literal),
    ExprString(Literal),
    ExprCollection(Vec<Self>),
    ExprTuple(Vec<Self>),
    ExprStruct(Vec<(Identifier, Option<Self>)>),
    ExprUnaryOperation(Operator, Box<Self>),
    ExprIdentifier(Identifier),
    ExprKewword(TokenKind),

    ExprWithMeta {
        expr: Box<Self>,
        id: meta::Id,
        typ: Option<Type>, 
    }
}

impl Expression {
    pub fn expr_do(self) -> Self {
        ExprDo(vec![StmtExpr(self)])
    }
}

#[derive(Debug, PartialEq, Clone, Hash)]
pub enum Pattern {
    PatStruct(Vec<Self>),
    PatTuple(Vec<Self>),
    PatNumber(Literal),
    PatString(Literal),
    PatCollection(Vec<Self>),
    PatWildcard,
    PatDotDot,
    PatLabeled(Identifier, Box<Self>),
    PatIdentifier(Identifier),
    PatValued(Identifier, Box<Self>),
    PatTyped(Box<Self>, Type),
    PatOr(Vec<Self>),
}

#[derive(Debug, PartialEq, Clone, Hash)]
pub enum Type {
    TypeEnum(Vec<(Identifier, Self)>),
    TypeStruct(Vec<(Identifier, Self)>),
    TypeTuple(Vec<Self>),
    TypeCollection(Box<Self>),
    TypeIdentifier(Identifier),
    TypeWithGenerics(Box<Self>, Vec<Self>),
    TypeFn(Box<Self>, Box<Self>),
    TypeKeyword(TokenKind),
}

#[derive(Debug, PartialEq, Clone, Hash)]
pub enum Modifier {
    ModPipe(Expression),
    ModAttribute(Expression),
    ModKeywordAttribute(TokenKind),
    ModCall(Expression),
    ModQuestion,
    ModBinaryOperation(Operator, Expression),
    ModGenerics(Vec<Type>),
}

pub type Operator = TokenKind;
pub type Identifier = String;
pub type Literal = String;

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn sizes() {
        println!("{:?}", std::mem::size_of::<Modifier>());
        println!("{:?}", std::mem::size_of::<Statement>());
        println!("{:?}", std::mem::size_of::<Expression>());
        println!("{:?}", std::mem::size_of::<Pattern>());
    }
}
