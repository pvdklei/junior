
module Ast where

import qualified Lex
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PChar

data Module = Module Header [Stmt] deriving (Show, Eq)
type Header = ()

data Stmt = 
    VarAssignment Pattern Expr       
    | VarTypeDecl VarIdent Type                        
    | VarTypeDeclAssign                                
        Pattern Type Expr            
    | TypeDef TypeIdent Type                 
    | TraitDef TypeIdent [Stmt] --TypeInherit                 
    | TraitImpl TypeIdent Trait [Stmt]                      
    | TypeImpl TypeIdent [Stmt]      
    deriving (Show, Eq)

data Expr = 
    IntLiteral Int
    | FloatLiteral Float
    | BoolLiteral Bool
    | StringLiteral String
    | InterpolatedStringLit InterpolatedString
    | VecLiteral [Expr]
    | TupleLiteral [Expr]
    | Var VarPath 
    | Lambda [Pattern] Expr
    | BinaryOp Expr BinaryOperator Expr
    | UnaryOp UnaryOperator Expr
    | Block [Stmt] Expr
    | FnCall VarPath [Expr]
    | Condition (Expr, Expr) [(Expr, Expr)] Expr
    | WhereExpr Expr [Stmt]
    | EnclosedExpr Expr -- in parenthesis
    | AnonStructConstructor Expr -- Expr.Block
    | TupleStructConstructor TypePath [Expr]
    | StructConstructor TypePath [Stmt] (Maybe Expr)
    | LabelConstructor TypePath
    | Match Expr [(Pattern, Expr)]
    | BooleanMatch Expr Pattern
    | DotTrail Expr [Expr]
    deriving (Show, Eq)

interpolateString :: String -> Maybe InterpolatedString
interpolateString s = case P.parse pStringInterpolate "" s of
    Right s -> Just s
    Left e -> Nothing

pStringInterpolate :: Lex.Lexer InterpolatedString
pStringInterpolate = do
    strexprs <- P.many $ pStrAndExpr
    str <- pStr
    return $ InterpolatedString strexprs str
    where pStr = P.many $ PChar.satisfy (/='$')
          pStrAndExpr = do
              str <- pStr
              PChar.satisfy (=='$')
              (Lex.VarIdentifier id) <- Lex.varIdentifier
              return $ (str, id)

data InterpolatedString = InterpolatedString [(String, Ast.VarIdent)] String
    deriving (Show, Eq)

data BinaryOperator = 
    Plus | Minus
    | Mult | Div
    | Modulo
    | And | Or | Less 
    | More | Leq | Meq 
    | Eq | Neq
    deriving (Show, Eq)

data Pattern = 
    -- DATA STRUCTS
    TupleDestr [Pattern]                     -- (_, thing, _)
    | ArrayDestr [Pattern]                   -- [pattern, ..]
    | StructDestr TypeIdent [Pattern]        -- MuStructOrEnum { id: pattern, id }
    | TupleStructDestr TypeIdent [Pattern]   -- MyTupleOrEnum (pattern, _, _)
    | LabelPattern TypeIdent                 -- EnumLabel
    | CatchPattern VarIdent Pattern          -- catch @ [12, ..]
    | IdValuePattern VarIdent Pattern        -- id: pattern
    | OrPattern [Pattern]                    -- pat1 | pat2 -> ...
    | Guarded Pattern Expr

    -- PRIMITIVES
    | StringLitPattern String
    | IntLitPattern Int
    | FloatLitPattern Float
    | BooleanLitPattern Bool
    | IdentPattern VarIdent
    | DotDotPattern                         -- ..
    | WildCardPattern                       -- _
    deriving (Show, Eq)

data UnaryOperator = 
    Negative | MakeRef | MakePtr
    deriving (Show, Eq)

lexBopToAstOp :: Lex.Token -> BinaryOperator
lexBopToAstOp op = case op of
    Lex.Minus -> Minus
    Lex.Plus -> Plus
    Lex.Mult -> Mult
    Lex.Div -> Div
    Lex.Modulo -> Modulo
    Lex.And -> And
    Lex.Or -> Or
    Lex.Eq -> Eq
    Lex.Leq -> Leq
    Lex.Meq -> Meq
    Lex.More -> More
    Lex.Less -> Less
    Lex.Neq -> Neq
    other -> error $ "Not a bin operator: " <> show other

lexUopToAstOp :: Lex.Token -> UnaryOperator
lexUopToAstOp op = case op of
    Lex.Minus -> Negative
    Lex.Mult -> MakePtr
    Lex.Ref -> MakeRef
    x -> error $ "Not an unary op: " <> show x

bopToStr :: BinaryOperator -> String
bopToStr op = case op of
    Minus -> "-"
    Plus -> "+"
    Div -> "/"
    Mult -> "*"
    Modulo -> "%" 
    Eq -> "=="
    Neq -> "!="
    Leq -> "<="
    Meq -> ">="
    Less -> "<"
    More -> ">"
    And -> "&&"
    Or -> "||"

data Type = 
    FnType [Type] Type
    | SimpleType TypePath
    | Tuple [Type]
    | Vec Type
    | String
    | Struct [Stmt] -- TypeInherit
    | TupleStruct [Type]
    | Enum [EnumOption]
    | Ref Type
    | Ptr Type
    | TraitObj TypeIdent
    deriving (Show, Eq)

data EnumOption = 
    Label TypeIdent
    | LabeledStruct [Stmt] TypeIdent -- this way around makes parsing easier
    | LabeledTuple [Type] TypeIdent
    deriving (Show, Eq)

type TypeInherit = TypeIdent

type Trait = TypeIdent
type Params t = Maybe [t]

data VarPath = VarPath (Maybe ModIdent) VarIdent 
    deriving (Eq, Show)
data TypePath = TypePath (Maybe ModIdent) TypeIdent
    deriving (Eq, Show)

type VarIdent = String
type ModIdent = String
type TypeIdent = String



