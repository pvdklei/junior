use crate::ast::{NodeType, ParseNode, AST};
use crate::lexer::{Lexer, Token, Span};
use crate::parse_error;
use itertools::{multipeek, MultiPeek};

pub struct Parser<'a> {
    lexer: MultiPeek<Lexer<'a>>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Self {
        let lexer = multipeek(lexer);
        Self { lexer }
    }

    pub fn parse(&mut self) -> AST {
        // parse program
        AST {
            top_node: self.parse_program(),
        }
    }

    fn parse_program(&mut self) -> ParseNode {
        let mut children = self.on_new_parse();

        while let Some((token, _)) = self.lexer.peek() {

            println!("{:?}", token);

            match token {
                // typeless assign and fncall
                Token::Ident(id) => {
                    // typeless assignment
                    match self.peek_nth(2) {
                        Some((Token::Equals, _)) => {
                            self.lexer.next();
                            children.push(self.parse_typeless_assignment());
                        },
                        _ => {}
                    }
                }

                // typed assignment
                Token::TypeId(id) => {}

                _ => {}
            }

        }
        ParseNode {
            entry: NodeType::Program,
            children,
        }
    }

    fn parse_typeless_assignment(&mut self) -> ParseNode {
        let mut children = self.on_new_parse();

        match self.lexer.next() {
            Some((Token::Ident(id), _)) => children.push(ParseNode {
                children: Vec::new(),
                entry: NodeType::Ident(id),
            }),
            _ => parse_error!("typeless asignment does not start with an id")
        }

        // skip '='
        self.lexer.next();

        children.push(self.parse_expression());

        ParseNode {
            children,
            entry: NodeType::TypeLessAssign,
        }
    }

    fn parse_expression(&mut self) -> ParseNode {
        let mut children = self.on_new_parse();

        match self.lexer.next() {

            // identifier
            Some((Token::Ident(id), _)) => {
                children.push(ParseNode {
                    children: Vec::new(),
                    entry: NodeType::Ident(id)
                })
            }
            
            // fncall


            _ => {}
        }

        ParseNode {
            children, 
            entry: NodeType::Expr
        }
    }

    fn on_new_parse(&mut self) -> Vec<ParseNode> {
        self.lexer.reset_peek();
        Vec::new()
    }

    fn peek_nth(&mut self, n: usize) -> Option<&(Token, Span)> {
        for _ in 0..n-1 {
            self.lexer.peek();
        }
        self.lexer.peek()
    }
}

#[macro_export]
macro_rules! parse_error {
    ($s:expr) => {
        println!("ParseError: {}", $s);
    };
}
