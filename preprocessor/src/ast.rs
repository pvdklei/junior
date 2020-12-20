pub struct AST {
    pub top_node: ParseNode
}

pub struct ParseNode {
    pub entry: NodeType,
    pub children: Vec<Self>
}

pub enum NodeType {
    Program,
    Statement,
    FnCall,
    Expr,
    TypedAssign,
    TypeLessAssign,
    Ident(String),
}
