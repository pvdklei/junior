use std::ops::Deref;

pub use TokenKind::*;
#[derive(Clone, Debug, PartialEq)]
pub enum TokenKind {
    // symbols
    Colon,
    Dot,
    Asterix,
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    OpenAngle,
    CloseAngle,
    Plus,
    Minus,
    And,
    Or,
    HashTag,
    Slash,
    Percentage,
    Question,
    Equals,
    Separator,
    Underscore,

    // kw control flow
    If,
    Else,
    Loop,
    Break,
    Continue,
    Match,
    SmallSelf,
    CapitalSelf,
    Const,
    Fn,
    Do,
    End,
    Then,
    As,

    // kw statement
    Type,
    Union,
    Enum,
    Trait,
    Struct,
    Let,
    Mut,

    // wk types
    Ref,
    Var,
    Val,
    String_,
    Str,
    Box_,
    Vec_,
    List,
    Some_,
    None_,
    Option_,
    Result_,
    Err_,
    Ok_,
    Bool,
    True,
    False,
    Iter,

    // other
    Copy,
    Clone,
    Move,
    Moved,
    Weak,
    Mod,
    Crate,
    Into,
    Jr,
    Rs,
    Float,
    Int,

    // compound
    Id(String),
    Quoted(String),
    Number(u64),

    // utils
    Nothing,
    Skip,
}

impl TokenKind {
    pub fn as_string(&self) -> String {
        format!("{:?}", self)
    }
}

#[derive(Debug)]
pub struct Token {
    pub from: (usize, usize),
    pub to: (usize, usize),
    pub kind: TokenKind,
}

struct Slice<'a> {
    pub src: &'a str,
    pub line: usize,
    pub column: usize,
}

impl<'a> Deref for Slice<'a> {
    type Target = str;
    fn deref(&self) -> &Self::Target {
        self.src
    }
}

impl<'a> Slice<'a> {
    fn new(src: &'a str) -> Self {
        Self {
            src,
            line: 0,
            column: 0,
        }
    }
    fn at(&self) -> (usize, usize) {
        (self.line, self.column)
    }
    fn newline(&mut self) {
        self.column = 0;
        self.line += 1;
    }
    fn slice(&mut self, at: usize) {
        self.column += at;
        self.src = &self.src[at..];
    }
}

#[allow(dead_code)]
pub fn map_kind(tks: &[Token]) -> Vec<TokenKind> {
    tks.iter().map(|t| t.kind.clone()).collect::<Vec<_>>()
}

#[derive(Debug)]
pub enum LexError {
    NoMatch,
    SyntaxError { line: usize, column: usize },
}

pub fn tokenize(src: &str) -> Result<Vec<Token>, LexError> {
    let mut src = Slice::new(src);
    let mut tks = Vec::new();
    loop {
        let from = src.at();
        let tkn = match lex_any(&mut src) {
            Ok(tkn) => tkn,
            Err(_) => {
                let (line, column) = src.at();
                return Err(LexError::SyntaxError { line, column });
            }
        };
        let to = src.at();
        if !(tkn == Skip) {
            tks.push(Token {
                kind: tkn,
                from,
                to,
            });
        }
        if src.is_empty() {
            break;
        }
    }
    Ok(tks)
}

fn lex_any(src: &mut Slice) -> Result<TokenKind, LexError> {
    for lexer in [lex_symbol, lex_kw, lex_quoted, lex_id, lex_number] {
        match lexer(src) {
            Nothing => continue,
            tkn => return Ok(tkn),
        }
    }
    Err(LexError::NoMatch)
}

macro_rules! lex_kw {
    ( $src:expr, $len:expr, $( $kw:pat => $tkn:expr ),* ) => {

        let out_of_src = $src.len() < $len;
        if out_of_src {
            return Nothing;
        }

        let ws_after = match $src.chars().nth($len) {
            Some(c) if c.is_ascii_whitespace() || c.is_ascii_punctuation() => true,
            None => true,
            _ => false
        };
        if ws_after {
            let kw = match &$src[..$len] {
                $(
                    $kw => $tkn,
                )*
                _ => Nothing
            };

            if kw != Nothing {
                $src.slice($len);
                return kw;
            }
        }

    };
}

fn lex_kw(src: &mut Slice) -> TokenKind {
    lex_kw!(src, 2, "if" => If, "fn" => Fn, "do" => Do, "as" => As, 
                    "jr" => Jr, "rs" => Rs, "ok" => Ok_);
    lex_kw!(src, 3, "let" => Let, "mut" => Mut, "end" => End, "str" => Str,
                    "ref" => Ref, "val" => Val, "var" => Var, "float" => Float,
                    "box" => Box_, "vec" => Vec_, "err" => Err_, "int" => Int);
    lex_kw!(src, 4, "self" => SmallSelf, "loop" => Loop, "Self" => CapitalSelf,
                    "else" => Else, "enum" => Enum, "type" => Type, "iter" => Iter,
                    "then" => Then, "list" => List, "true" => True, "false" => False,
                    "none" => None_, "some" => Some_, "bool" => Bool);
    lex_kw!(src, 5, "match" => Match, "break" => Break, "const" => Const, 
                    "trait" => Trait, "union" => Union);
    lex_kw!(src, 6, "struct" => Struct, "result" => Result_, 
                    "option" => Option_, "string" => String_);
    lex_kw!(src, 8, "continue" => Continue);
    return Nothing;
}

fn lex_symbol(src: &mut Slice) -> TokenKind {
    if src.is_empty() {
        return Nothing;
    }
    let sym = match src.chars().next().unwrap() {
        '+' => Plus,
        '-' => Minus,
        '*' => Asterix,
        '/' => Slash,
        '&' => And,
        '|' => Or,
        '=' => Equals,
        '<' => OpenAngle,
        '>' => CloseAngle,
        '(' => OpenParen,
        ')' => CloseParen,
        '{' => OpenBrace,
        '}' => CloseBrace,
        '[' => OpenBracket,
        ']' => CloseBracket,
        '_' => Underscore,
        '?' => Question,
        '%' => Percentage,
        '.' => Dot,
        ':' => Colon,
        ',' => Separator,
        '\n' => {
            src.newline();
            Separator
        }
        ' ' | '\t' => Skip,
        _ => Nothing,
    };

    if !(sym == Nothing) {
        src.slice(1);
        return sym;
    }
    return Nothing;
}

fn lex_quoted(src: &mut Slice) -> TokenKind {
    let mut cs = src.chars();
    match cs.next().unwrap() {
        '"' => {}
        _ => return Nothing,
    }
    let content = cs.take_while(|c| *c != '"').collect::<String>();
    let skip = content.len() + 2;
    src.slice(skip);
    Quoted(content)
}

fn lex_id(src: &mut Slice) -> TokenKind {
    let first = match src.chars().next() {
        Some(c) => c,
        None => return Nothing,
    };
    if !(first.is_alphabetic() || first == '_') {
        return Nothing;
    }
    let id = src
        .chars()
        .take_while(|c| c.is_alphabetic() || *c == '_' || c.is_numeric())
        .collect::<String>();
    src.slice(id.len());
    Id(id)
}

fn lex_number(src: &mut Slice) -> TokenKind {
    let numbers = src
        .chars()
        .take_while(|c| c.is_numeric())
        .collect::<String>();
    match numbers.parse::<u64>() {
        Ok(n) => {
            src.slice(numbers.len());
            Number(n)
        }
        _ => Nothing,
    }
}

#[cfg(test)]
mod tests {
    use std::vec;

    use super::*;
    macro_rules! test_lexer {
        ($src:expr, $tks:expr) => {
            assert_eq!(map_kind(&tokenize($src).unwrap()), $tks)
        };
    }

    #[test]
    fn do_() {
        test_lexer!(
            "do, let a = 2, end ",
            vec![
                Do,
                Separator,
                Let,
                Id("a".into()),
                Equals,
                Number(2),
                Separator,
                End
            ]
        );
    }

    #[test]
    fn ifelse() {
        test_lexer!("if expr, expr else expr", {
            let expr = Id("expr".into());
            vec![If, expr.clone(), Separator, expr.clone(), Else, expr]
        })
    }
}
