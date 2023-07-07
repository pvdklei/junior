
module Lex (tokenize, Token(..), Lexer, varIdentifier) where

import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PChar
import qualified Text.Parsec.Combinator as PCom
import qualified Text.Parsec.String as PStr

import qualified Control.Monad as M
import qualified Data.Char as Char

type Lexer = P.Parsec String ()
type TLexer = Lexer Token

data Token =
    -- IDENTIFIERS
    VarIdentifier String            -- starts with lowercase 
    | TypeIdentifier String         -- starts with cap
    | DotVarIdentifier String       -- starts with . and then lowercase

    -- LITERALS
    | StringLiteral String          -- "my string ..." 
    | IntLiteral Int                -- digits
    | FloatLiteral Float            -- digits 'dot' digits

    -- PRIMITIVES
    | Separator                     -- comma or newline
    | Colon                         -- ':' 
    | Rarrow                        -- '->' 
    | Equals                        -- '='
    | OpenBrace | CloseBrace        -- {}
    | OpenParen | CloseParen        -- ()
    | OpenBracket | CloseBracket    -- ()
    | Dot                           -- '.'
    | Ref                           -- '&'
    | DotDot
    | Underscore
    | MonkeyTail
    | Bar
    | And | Or | Less 
    | More | Leq | Meq | Eq | Neq

    -- KEYWORDS 
    | Struct
    | Type
    | Enum
    | If
    | Else
    | Then
    | Elif
    | Trait
    | Match
    | As
    | Impl
    | For
    | Where
    | TrueKw
    | FalseKw

    -- OPERATORS
    | Plus | Minus
    | Mult | Div
    | Modulo

    | Error                         -- when nothing fits
    deriving (Show, Eq)

tokenize :: String -> [Token]
tokenize src = case lexOutcome of
    Left err -> error $ show err
    Right tks -> tks
    where lexOutcome = P.parse moduleLexer "" src

moduleLexer :: Lexer [Token]
moduleLexer = do
    -- P.skipMany whiteSpace
    tks <- PCom.many1 tokenLexer
    P.eof
    return tks

tokenLexer :: TLexer
tokenLexer = do
    tkn <- stringLit
        P.<|> P.try floatLit
        P.<|> integerLit

        -- SYMBOLS
        P.<|> colon
        P.<|> P.try rarrow
        P.<|> separator
        P.<|> stringLit
        P.<|> P.try boolOp
        P.<|> equals
        P.<|> plus
        P.<|> minus
        P.<|> mult
        P.<|> divide
        P.<|> modulo
        P.<|> P.try dotdot
        P.<|> dot
        P.<|> ref
        P.<|> underscore
        P.<|> monkeyTail
        P.<|> bar

        -- ENCLOSINGS
        P.<|> openBrace
        P.<|> closeBrace
        P.<|> openParen
        P.<|> closeParen
        P.<|> openBracket
        P.<|> closeBracket

        -- KWS
        P.<|> P.try boolKw
        P.<|> P.try ifKw
        P.<|> P.try elseKw
        P.<|> P.try thenKw
        P.<|> P.try elifKw
        P.<|> P.try whereKw
        P.<|> P.try structKw
        P.<|> P.try enumKw
        P.<|> P.try traitKw
        P.<|> P.try implKw
        P.<|> P.try matchKw
        P.<|> P.try typeKw
        P.<|> P.try asKw


        -- ID
        P.<|> typeIdentifier
        P.<|> varIdentifier
    P.skipMany whiteSpace
    return tkn

-- IDENTIFIERS

typeIdentifier :: TLexer
typeIdentifier = do
    head <- PChar.satisfy Char.isUpper
    tail <- P.many identChar
    return $ TypeIdentifier (head : tail)

varIdentifier :: TLexer
varIdentifier = do
    head <- PChar.satisfy Char.isLower
    tail <- P.many identChar
    return $ VarIdentifier (head : tail)

-- LITERALS

stringLit :: TLexer
stringLit = do
    skipQuote
    s <- P.many $ PChar.satisfy isNotQuote
    skipQuote
    return $ StringLiteral s
    where
        skipQuote = M.void $ PChar.char '"'
        isNotQuote '"' = False
        isNotQuote _ = True

floatLit :: TLexer
floatLit = do befdot <- P.many1 PChar.digit
              M.void $ PChar.char '.'
              afterdot <- P.many1 PChar.digit
              return $ FloatLiteral $ read $ befdot <> "." <> afterdot <> "0"

integerLit :: TLexer
integerLit = do int <- PCom.many1 PChar.digit
                return $ IntLiteral $ read int

-- SIMPLE

whiteSpace :: Lexer ()
whiteSpace = skipWs >> return ()
    where skipWs = (M.void $ PChar.oneOf "\t ") 
                    P.<|> singleLineComment

separator :: TLexer
separator = do PCom.many1 $ PChar.oneOf "\n,"
               return Separator

colon = PChar.char ':' >> return Colon
rarrow = PChar.string "->" >> return Rarrow
equals = PChar.char '=' >> return Equals
openBrace = singleChar '{' OpenBrace
closeBrace = singleChar '}' CloseBrace
openParen = singleChar '(' OpenParen
closeParen = singleChar ')' CloseParen
openBracket = singleChar '[' OpenBracket
closeBracket = singleChar ']' CloseBracket
dotdot = string ".." DotDot
underscore = singleChar '_' Underscore
monkeyTail = singleChar '@' MonkeyTail
bar = singleChar '|' Bar

boolOp = eq 
    P.<|> P.try leq
    P.<|> P.try meq
    P.<|> neq
    P.<|> less
    P.<|> more
    P.<|> and_
    P.<|> or_
eq = string "==" Eq
leq = string "<=" Leq   
meq = string ">=" Meq
less = singleChar '<' Less
more = singleChar '>' More
and_ = string "&&" And
or_ = string "||" Or
neq = string "/=" Neq

-- KEYWORDS

ifKw = string "if " If
thenKw = string "then " Then
elseKw = string "else " Else
elifKw = string "elif " Elif
whereKw = string "where " Where
structKw = string "struct " Struct 
typeKw = string "type " Type
asKw = string "as " As

enumKw = string "enum " Enum
traitKw = string "trait " Trait
implKw = string "impl " Impl
matchKw = string "match " Match
boolKw = string "true " TrueKw 
         P.<|> string "false " FalseKw

-- OPERATORS

plus = singleChar '+' Plus
minus = singleChar '-' Minus
divide = singleChar '/' Div
mult = singleChar '*' Mult
modulo = singleChar '%' Modulo
dot = singleChar '.' Dot
ref = singleChar '&' Ref

-- COMMENTS

singleLineComment :: Lexer ()
singleLineComment = do
        PChar.string "//"
        P.many $ PChar.satisfy (/='\n')
        return ()

-- UTILS

singleChar :: Char -> Token -> TLexer
singleChar c t = PChar.char c >> return t

string :: String -> Token -> TLexer
string s t = PChar.string s >> return t

identChar :: Lexer Char
identChar = PChar.alphaNum P.<|> PChar.oneOf "_"

debug :: Show a => a -> Lexer b
debug = fail . show






