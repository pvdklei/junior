
use plex::lexer;

#[derive(Debug, Clone)]
pub enum Token {
    Ident(String),
    TypeId(String),
    Integer(i32),
    Float(f64),
    String(String),
    Equals,
    Plus,
    Minus,
    Star,
    Slash,
    LParen,
    RParen,
    NewLine,
    Whitespace,
    Comment,
}

lexer! {
    pub fn next_token(text: 'a) -> Token;

    r#"[ \t\r]+"# => Token::Whitespace,
    r#"[\n]+"# => Token::NewLine,

    r#"//[^\n]*"# => Token::Comment,

    r#"[0-9]+"# => {
        if let Ok(f) = text.parse() {
            Token::Integer(f)
        } else {
            panic!("integer {} is out of range", text)
        }
    }

    r#"[0-9]+\.[0-9]+"# => {
        if let Ok(i) = text.parse() {
            Token::Float(i)
        } else {
            panic!("could not parse float {} - too large prob", text)
        }
    }
    r#"["'].*["']"# => Token::String(text.to_owned()),
    r#"[a-z][a-zA-Z0-9]*"# => Token::Ident(text.to_owned()),
    r#"[A-Z][a-zA-Z0-9]*"# => Token::TypeId(text.to_owned()),

    r#"="# => Token::Equals,
    r#"\+"# => Token::Plus,
    r#"-"# => Token::Minus,
    r#"\*"# => Token::Star,
    r#"/"# => Token::Slash,
    r#"\("# => Token::LParen,
    r#"\)"# => Token::RParen,

    r#"."# => panic!("unexpected character: {}", text),
}

pub struct Lexer<'a> {
    original: &'a str,
    remaining: &'a str,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Lexer<'a> {
        Lexer {
            original: s,
            remaining: s,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl<'a> Iterator for Lexer<'a> {
    type Item = (Token, Span);
    fn next(&mut self) -> Option<(Token, Span)> {
        loop {
            let (tok, span) = if let Some((tok, new_remaining)) = next_token(self.remaining) {
                let lo = self.original.len() - self.remaining.len();
                let hi = self.original.len() - new_remaining.len();
                self.remaining = new_remaining;
                (tok, Span { lo, hi })
            } else {
                return None;
            };
            match tok {
                Token::Whitespace | Token::Comment | Token::NewLine => {
                    continue;
                }
                tok => {
                    return Some((tok, span));
                }
            }
        }
    }
}
