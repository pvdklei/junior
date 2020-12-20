mod lexer;
mod parser;
mod ast;

use std::fs::{
    read_to_string,
};
use lexer::Lexer;
use parser::Parser;

fn main() {

    // let mut iter = itertools::multipeek(vec![1, 2, 3, 4, 5, 6, 7,]);
    // println!("{:?}", iter.nth(1));
    // println!("{:?}", iter.peek());
    // println!("{:?}", iter.peek());
    // println!("{:?}", iter.next());
    // println!("{:?}", iter.peek());

    let file = read_to_string("../test.jr").expect("Could not find file");
    let mut lexer = Lexer::new(&file);
    let mut parser = Parser::new(lexer);
    let mut ast = parser.parse();
}




