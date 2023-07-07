{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Parse (buildTree) where

import qualified Lex
import qualified Ast

import qualified Text.Parsec as P
import qualified Text.Parsec.Combinator as PCom
import qualified Text.Parsec.Prim as PPrim

import qualified Control.Monad as M
import qualified Debug.Trace as Debug

import Data.Maybe

type Parser = P.Parsec [Lex.Token] ()

buildTree :: [Lex.Token] -> Ast.Module
buildTree tks = case P.parse pMod "" tks of
    Right mod -> mod
    Left err -> error $ show err

pMod :: Parser Ast.Module
pMod = do
    skipSeparators
    stmts <- PCom.many1 pStmt
    P.eof
    return $ Ast.Module () stmts

-- STATEMENTS

pStmt :: Parser Ast.Stmt
pStmt = do
    stmt <- P.try pVarTypeDeclAssign
        P.<|> P.try pVarTypeDecl
        P.<|> P.try pVarAssignment
        P.<|> pTraitDef
        P.<|> pTypeDef
        P.<|> pTypeImpl
    skipSeparators
    return stmt

pVarTypeDeclAssign :: Parser Ast.Stmt
pVarTypeDeclAssign = do
    var <- pVarIdent
    -- params <- P.optionMaybe pFnParams
    M.void $ satisfy (==Lex.Colon)
    ty <- pType
    M.void $ satisfy (==Lex.Equals)
    Ast.VarTypeDeclAssign (Ast.IdentPattern var) ty <$> pVarAssignmentExpr

pVarAssignment :: Parser Ast.Stmt
pVarAssignment = do
    var <- pAssignmentPattern 
    -- params <- P.optionMaybe pFnParams
    M.void $ satisfy (==Lex.Equals)
    Ast.VarAssignment var <$> pVarAssignmentExpr

pAssignmentPattern :: Parser Ast.Pattern
pAssignmentPattern = do
    pIdPattern
    P.<|> pWildcardPattern
    P.<|> P.try pDestructurePattern

pVarTypeDecl :: Parser Ast.Stmt
pVarTypeDecl = do
    var <- pVarIdent
    M.void $ satisfy (==Lex.Colon)
    Ast.VarTypeDecl var <$> pType

pTypeDef :: Parser Ast.Stmt
pTypeDef = do
    satisfy (==Lex.Type)
    id <- pTypeIdent
    ty <- P.try pNewType P.<|> pType
    return $ Ast.TypeDef id ty


pTraitDef :: Parser Ast.Stmt
pTraitDef = do
    satisfy (==Lex.Trait)
    id <- pTypeIdent
    stmts <- pEnclosedAndSeparated Lex.OpenBrace Lex.CloseBrace Lex.Separator pStmt
    return $ Ast.TraitDef id stmts

pTraitImpl :: Parser Ast.Stmt
pTraitImpl = do
    satisfy (==Lex.Impl)
    tyid <- pTypeIdent
    satisfy (==Lex.As)
    traitid <- pTrait
    stmts <- pEnclosedAndSeparated Lex.OpenBrace Lex.CloseBrace Lex.Separator pStmt
    return $ Ast.TraitImpl tyid traitid stmts

pTypeImpl :: Parser Ast.Stmt
pTypeImpl = do
    satisfy (==Lex.Impl)
    id <- pTypeIdent
    stmts <- pEnclosedAndSeparated Lex.OpenBrace Lex.CloseBrace Lex.Separator pStmt
    return $ Ast.TypeImpl id stmts

-- EXPRESIONS

pExpr :: Parser Ast.Expr
pExpr = do P.try pBinOp P.<|> pDotTrail


-- to prevent infinite recursion the first expr 
-- in a binop cannot be a binop as well
pExprNoBinOpAndDotTrail :: Parser Ast.Expr
pExprNoBinOpAndDotTrail = do
    pIntLit
        P.<|> pUnOp
        P.<|> P.try pStructConstructor
        P.<|> P.try pTupleStructConstructor
        P.<|> P.try pEnumLabelConstructor
        P.<|> pFloatLit
        P.<|> pStringLit
        P.<|> pVecLit
        P.<|> pBoolean
        P.<|> P.try pFnCall
        P.<|> pCondition
        P.<|> pMatch
        P.<|> P.try pLambda
        P.<|> P.try pTuple
        P.<|> pEnclosedExpr
        P.<|> pVar
        P.<|> P.try pAnonStructConstructor
        P.<|> pBlock

pBinOp :: Parser Ast.Expr
pBinOp = do
    expr <- pDotTrail 
    op <- pBinOperator
    Ast.BinaryOp expr op <$> pExpr

pDotTrail :: Parser Ast.Expr
pDotTrail = do
    expr <- pExprNoBinOpAndDotTrail 
    dot <- P.optionMaybe $ P.try pDot 
    case dot of
        Just _ -> do
            dotexprs <- pEnclosedAndSeparated2 pNone pNone (P.try pDot) (P.try pDotExpr)
            return $ Ast.DotTrail expr dotexprs
        Nothing -> return expr 
    where pDotExpr = do
            dotexpr <- pIntLit
                P.<|> P.try pFnCallNoPath
                P.<|> pVarNoPath
            return $ dotexpr 
          pDot = do
              P.many $ satisfy (==Lex.Separator) 
              satisfy (==Lex.Dot) 
              return ()

pBoolean :: Parser Ast.Expr
pBoolean = do
    bool <- satisfy isBool
    return $ Ast.BoolLiteral $ case bool of
        Lex.TrueKw -> True
        Lex.FalseKw -> False
        _ -> error "Cannot happen (pBool)"
    where isBool Lex.TrueKw = True
          isBool Lex.FalseKw = True
          isBool _ = False

-- same as pExpr buts adds a possible where clause and 
-- extra separators (only after the expr in an assignment)
pVarAssignmentExpr :: Parser Ast.Expr
pVarAssignmentExpr = do
    expr <- pExpr 
    skipSeparators
    mb <- P.optionMaybe pWhereExpr
    skipSeparators
    return $ case mb of
        Just stmts -> Ast.WhereExpr expr stmts
        Nothing -> expr

pEnclosedExpr :: Parser Ast.Expr
pEnclosedExpr = do
    satisfy (==Lex.OpenParen)
    expr <- pExpr
    satisfy (==Lex.CloseParen)
    return $ Ast.EnclosedExpr expr

pWhereExpr :: Parser [Ast.Stmt]
pWhereExpr = do
    satisfy (==Lex.Where)
    satisfy (==Lex.OpenBrace)
    skipSeparators
    stmts <- P.many pStmt
    satisfy (==Lex.CloseBrace)
    skipSeparators
    return stmts

pIntLit :: Parser Ast.Expr
pIntLit = do
    Lex.IntLiteral int <- satisfy isInt
    return $ Ast.IntLiteral int
    where isInt (Lex.IntLiteral _) = True
          isInt _ = False

pFloatLit :: Parser Ast.Expr
pFloatLit = do
    Lex.FloatLiteral float <- satisfy isFloat
    return $ Ast.FloatLiteral float
    where isFloat (Lex.FloatLiteral _) = True
          isFloat _ = False

pStringLit :: Parser Ast.Expr
pStringLit = do
    Lex.StringLiteral str <- satisfy isStr
    case Ast.interpolateString str of
        Just is -> return $ Ast.InterpolatedStringLit is
        Nothing -> return $ Ast.StringLiteral str
    where isStr (Lex.StringLiteral _) = True
          isStr _ = False

pVecLit :: Parser Ast.Expr
pVecLit = do
    exprs <- pEnclosedAndSeparated Lex.OpenBracket
            Lex.CloseBracket Lex.Separator pExpr
    return $ Ast.VecLiteral exprs

pTuple :: Parser Ast.Expr
pTuple = do
    exprs <- pEnclosedAndSeparated Lex.OpenParen Lex.CloseParen Lex.Separator pExpr
    return $ Ast.TupleLiteral exprs

pLambda :: Parser Ast.Expr
pLambda = do
    pats <- P.optionMaybe $ pEnclosedAndSeparated Lex.OpenParen Lex.CloseParen Lex.Separator pPattern
    case pats of 
        Just ps -> do
            satisfy (==Lex.Rarrow)
            Ast.Lambda ps <$> pExpr
        Nothing -> do
            pat <- pPattern
            satisfy (==Lex.Rarrow)
            Ast.Lambda [pat] <$> pExpr

pTupleStructConstructor :: Parser Ast.Expr
pTupleStructConstructor = do
    id <- pTypePath 
    (Ast.TupleLiteral exprs) <- pTuple
    return $ Ast.TupleStructConstructor id exprs

pAnonStructConstructor :: Parser Ast.Expr
pAnonStructConstructor = do
    satisfy (==Lex.OpenBrace)  
    satisfy (==Lex.CloseBrace)
    block <- pBlock
    return $ Ast.AnonStructConstructor block

pStructConstructor :: Parser Ast.Expr
pStructConstructor = do
    id <- pTypePath 
    satisfy (==Lex.OpenBrace)
    skipSeparators
    stmts <- P.many1 pStmt
    dd <- P.optionMaybe $ satisfy (==Lex.DotDot) 
    unrav <- case dd of
        Nothing -> return $ Nothing
        Just _ -> do
            expr <- P.optionMaybe $ pExpr
            case expr of
                Nothing -> return $ Just $ Ast.FnCall (Ast.VarPath Nothing "Default::default") []
                Just expr' -> return $ Just expr'
    satisfy (==Lex.CloseBrace)
    return $ Ast.StructConstructor id stmts unrav

pEnumLabelConstructor :: Parser Ast.Expr
pEnumLabelConstructor = do Ast.LabelConstructor <$> pTypePath


pUnOp :: Parser Ast.Expr
pUnOp = do
    op <- pUnaryOperator
    Ast.UnaryOp op <$> pExpr

pVar :: Parser Ast.Expr
pVar = do
    Ast.Var <$> pVarPath

pFnCall :: Parser Ast.Expr
pFnCall = do
    id <- pVarPath  
    exprs <- pEnclosedAndSeparated Lex.OpenParen Lex.CloseParen Lex.Separator pExpr
    return $ Ast.FnCall id exprs

pVarNoPath :: Parser Ast.Expr
pVarNoPath = do
    id <- pVarIdent
    return $ Ast.Var (Ast.VarPath Nothing id)

pFnCallNoPath :: Parser Ast.Expr
pFnCallNoPath = do
    id <- pVarIdent
    exprs <- pEnclosedAndSeparated Lex.OpenParen Lex.CloseParen Lex.Separator pExpr
    return $ Ast.FnCall (Ast.VarPath Nothing id) exprs

pBlock :: Parser Ast.Expr
pBlock = do
    satisfy (==Lex.OpenBrace)
    skipSeparators
    stmts <- P.many pStmt
    skipSeparators
    expr <- P.optionMaybe pExpr
    skipSeparators
    satisfy (==Lex.CloseBrace)
    return $ Ast.Block stmts $ case expr of
        Just expr -> expr
        Nothing -> Ast.TupleLiteral []

pCondition :: Parser Ast.Expr
pCondition = do
    if' <- pIf
    elifs <- P.many pElif
    Ast.Condition if' elifs <$> pElse
    where
        pIf = do
            skipSeparators
            satisfy (==Lex.If); skipSeparators
            cond <- pExpr; skipSeparators
            satisfy (==Lex.Then); skipSeparators
            expr <- pExpr; skipSeparators
            return (cond, expr)
        pElse = do
            skipSeparators
            satisfy (==Lex.Else); skipSeparators
            pExpr
        pElif = do
            skipSeparators
            satisfy (==Lex.Elif); skipSeparators
            cond <- pExpr; skipSeparators
            satisfy (==Lex.Then); skipSeparators
            expr <- pExpr; skipSeparators
            return (cond, expr)

pMatch :: Parser Ast.Expr
pMatch = do
    satisfy (==Lex.Match)
    expr <- pExpr
    body <- P.try pNormalMatch P.<|> pBoolMatch
    return $ body expr
    where
        pMatchArm = do
            pat <- pPattern
            satisfy (==Lex.Rarrow)
            expr <- pExpr
            return (pat, expr)
        pNormalMatch = do
            arms <- pEnclosedAndSeparated Lex.OpenBrace Lex.CloseBrace Lex.Separator pMatchArm
            return $ \x -> Ast.Match x arms
        pBoolMatch = do
            satisfy (==Lex.OpenBrace); skipSeparators
            pat <- pPattern
            skipSeparators; satisfy (==Lex.CloseBrace)
            return $ \x -> Ast.BooleanMatch x pat

-- TYPES

pType :: Parser Ast.Type
pType = do
    ty <- P.optionMaybe $ 
        P.try pFnType
        P.<|> pSimpleType
        P.<|> pTupleType
        P.<|> pVecType
        P.<|> pStringType
        P.<|> pTraitObjType
    case ty of
        Just ty' -> return $ ty'
        Nothing -> do -- else try refs and ptrs
            ref <- oneOf [Lex.Ref, Lex.Mult] 
            ty <- P.try pFnType
                P.<|> pSimpleType
                P.<|> pTupleType
                P.<|> pVecType
                P.<|> pStringType
                P.<|> pTraitObjType
            return $ case ref of
                Lex.Mult -> Ast.Ptr ty
                Lex.Ref -> Ast.Ref ty
                _ -> error "Impossible"

pStringType :: Parser Ast.Type
pStringType = do
    satisfy (==Lex.StringLiteral "") 
    return Ast.String

pTypeNoFn :: Parser Ast.Type
pTypeNoFn = do
    ref <- P.optionMaybe $ oneOf [Lex.Ref, Lex.Mult]
    ty <- pSimpleType
        P.<|> pTupleType
        P.<|> pVecType
        P.<|> pStringType
        P.<|> pTraitObjType
    return $ case ref of
        Just Lex.Mult -> Ast.Ptr ty
        Just Lex.Ref -> Ast.Ref ty
        Just _ -> error "Impossible"
        Nothing -> ty

pSimpleType = Ast.SimpleType <$> pTypePath
pFnType = do
    tys <- P.optionMaybe $ pEnclosedAndSeparated Lex.OpenParen Lex.CloseParen Lex.Separator pType
    case tys of
        Just tys' -> do
            M.void $ satisfy (==Lex.Rarrow)
            Ast.FnType tys' <$> pType
        Nothing -> do
            ty <- pTypeNoFn 
            M.void $ satisfy (==Lex.Rarrow)
            Ast.FnType [ty] <$> pTypeNoFn
pTupleType = do
    tys <- pEnclosedAndSeparated Lex.OpenParen Lex.CloseParen Lex.Separator pType
    return $ Ast.Tuple tys
pVecType = do
    M.void $ satisfy (==Lex.OpenBracket)
    ty <- pType
    M.void $ satisfy (==Lex.CloseBracket)
    return $ Ast.Vec ty
pTraitObjType = do
    satisfy (==Lex.Impl)
    Ast.TraitObj <$> pTypeIdent

pNewType :: Parser Ast.Type
pNewType = P.try pEnum
        P.<|> pStruct
        P.<|> pTupleStruct
    where
        pStruct = do
            stmts <- pEnclosedAndSeparated Lex.OpenBrace Lex.CloseBrace Lex.Separator pStmt
            return $ Ast.Struct stmts
        pTupleStruct = do
            tys <- pEnclosedAndSeparated Lex.OpenParen Lex.CloseParen Lex.Separator pType
            return $ Ast.TupleStruct tys
        pEnum = do
            options <- pEnclosedAndSeparated Lex.OpenBrace Lex.CloseBrace Lex.Separator pEnumOption
            return $ Ast.Enum options
        pEnumOption = do
            label <- pTypeIdent
            body <- P.optionMaybe $ pStructOption P.<|> pTupleOption
            return $ case body of
                Nothing -> Ast.Label label
                Just e -> e label
            where
                pStructOption = do
                    stmts <- pEnclosedAndSeparated Lex.OpenBrace Lex.CloseBrace Lex.Separator pStmt
                    return $ Ast.LabeledStruct stmts
                pTupleOption = do
                    tys <- pEnclosedAndSeparated Lex.OpenParen Lex.CloseParen Lex.Separator pType
                    return $ Ast.LabeledTuple tys


pTrait :: Parser Ast.Trait
pTrait = pTypeIdent

-- PATTERN

pPattern :: Parser Ast.Pattern
pPattern = do
    pat <- pSimplePat
    ors <- P.many pBarAndPat
    pat <- case ors of
        [] -> return pat
        _ -> return $ Ast.OrPattern (pat:ors)
    guardExpr <- P.optionMaybe pGuard
    case guardExpr of
        Just expr -> return $ Ast.Guarded pat expr
        Nothing -> return pat
    where
        pLabelPattern = do
            Ast.LabelPattern <$> pTypeIdent
        pDotDotPattern = do
            satisfy (==Lex.DotDot)
            return Ast.DotDotPattern
        pCatchPattern = do
            id <- pVarIdent
            satisfy (==Lex.MonkeyTail)
            Ast.CatchPattern id <$> pPattern
        pSimplePat = do
            pLiteralPattern
            P.<|> pDotDotPattern
            P.<|> pWildcardPattern
            P.<|> P.try pDestructurePattern
            P.<|> P.try pCatchPattern
            P.<|> pIdPattern
            P.<|> pLabelPattern
        pBarAndPat = do
            satisfy (==Lex.Bar)
            pPattern
        pGuard = do
            satisfy (==Lex.If)
            pExpr

pIdPattern = do
    Ast.IdentPattern <$> pVarIdent
pWildcardPattern = do
    satisfy (==Lex.Underscore)
    return Ast.WildCardPattern

pLiteralPattern = do
    pIntPattern
    P.<|> pBooleanPattern
    P.<|> pFloatPattern
    P.<|> pStringPattern
    where
        pIntPattern = do
            (Lex.IntLiteral int) <- satisfy isInt
            return $ Ast.IntLitPattern int
            where isInt t = case t of
                    Lex.IntLiteral _ -> True
                    _ -> False
        pFloatPattern = do
            (Lex.FloatLiteral float) <- satisfy isFloat
            return $ Ast.FloatLitPattern float
            where isFloat t = case t of
                    Lex.FloatLiteral _ -> True
                    _ -> False
        pStringPattern = do
            (Lex.StringLiteral str) <- satisfy isStr
            return $ Ast.StringLitPattern str
            where isStr t = case t of
                    Lex.StringLiteral _ -> True
                    _ -> False
        pBooleanPattern = do
            (Ast.BoolLiteral bool) <- pBoolean
            return $ Ast.BooleanLitPattern bool

pDestructurePattern :: Parser Ast.Pattern
pDestructurePattern =
    pTuplePattern
    P.<|> pArrayPattern
    P.<|> P.try pTupleStructPattern
    P.<|> pStructPattern
    where
        pTuplePattern = do
            ptns <- pEnclosedAndSeparated Lex.OpenParen Lex.CloseParen Lex.Separator pPattern
            return $ Ast.TupleDestr ptns
        pArrayPattern = do
            ptns <- pEnclosedAndSeparated Lex.OpenBracket Lex.CloseBracket Lex.Separator pPattern
            return $ Ast.ArrayDestr ptns
        pTupleStructPattern = do
            id <- pTypeIdent
            ptns <- pEnclosedAndSeparated Lex.OpenParen Lex.CloseParen Lex.Separator pPattern
            return $ Ast.TupleStructDestr id ptns
        pStructPattern = do
            id <- pTypeIdent
            ptns <- pEnclosedAndSeparated Lex.OpenBrace
                    Lex.CloseBrace Lex.Separator (P.try pIdValuePattern P.<|> pPattern)
            return $ Ast.StructDestr id ptns
        pIdValuePattern = do
            id <- pVarIdent
            satisfy (==Lex.Equals)
            Ast.IdValuePattern id <$> pPattern



-- PRIMITIVES

pBinOperator :: Parser Ast.BinaryOperator
pBinOperator = Ast.lexBopToAstOp <$> oneOf
    [Lex.Plus, Lex.Minus, Lex.Mult, Lex.Div, Lex.Modulo, Lex.Eq,
     Lex.Meq, Lex.Leq, Lex.More, Lex.Less, Lex.And, Lex.Or, Lex.Neq]

pUnaryOperator :: Parser Ast.UnaryOperator
pUnaryOperator = Ast.lexUopToAstOp <$> oneOf [Lex.Minus, Lex.Mult, Lex.Ref]

pFnParams :: Parser [Ast.Pattern]
pFnParams = pEnclosedAndSeparated Lex.OpenParen
        Lex.CloseParen Lex.Separator pPattern

pTypePath :: Parser Ast.TypePath
pTypePath = do
    mod <- P.optionMaybe $ P.try pModPrefix 
    id <- pTypeIdent 
    case mod of
        Just mod' -> return $ Ast.TypePath (Just mod') id
        Nothing -> return $ Ast.TypePath Nothing id

pVarPath :: Parser Ast.VarPath
pVarPath = do
    mod <- P.optionMaybe $ P.try pModPrefix 
    id <- pVarIdent
    case mod of
        Just mod' -> return $ Ast.VarPath (Just mod') id
        Nothing -> return $ Ast.VarPath Nothing id

pModPrefix :: Parser Ast.ModIdent
pModPrefix = do
    id <- pVarIdent 
    satisfy (==Lex.Dot)
    return id  

pVarIdent :: Parser Ast.VarIdent
pVarIdent = do
    (Lex.VarIdentifier id) <- satisfy isVarID
    return $ id
    where isVarID tk = case tk of
            Lex.VarIdentifier _ -> True
            _ -> False


pTypeIdent :: Parser Ast.TypeIdent
pTypeIdent = do
    (Lex.TypeIdentifier id) <- satisfy isVarID
    return id
    where isVarID tk = case tk of
            Lex.TypeIdentifier _ -> True
            _ -> False

-- UTILS

satisfy :: (Lex.Token -> Bool) -> Parser Lex.Token
satisfy cond = PPrim.tokenPrim show next test
    where test t = if cond t then Just t else Nothing
          next pos _ _ = pos

oneOf :: [Lex.Token] -> Parser Lex.Token
oneOf molds = satisfy (`elem` molds)

skipSeparators :: Parser ()
skipSeparators = P.skipMany $ satisfy (==Lex.Separator)

pNone :: Parser ()
pNone = do return ()

debug :: Show a => a -> Parser b
debug = fail . (<> "\n") . ("\nParse Debug: " <>). show

pEnclosedAndSeparated ::
    Show t =>
    Lex.Token
    -> Lex.Token
    -> Lex.Token
    -> Parser t
    -> Parser [t]
pEnclosedAndSeparated open close sep parser = do
    M.void $ satisfy (==open) 
    skipSep
    elements <- P.many $ P.try pElemAndSep
    skipSep
    lastElement <- P.optionMaybe parser
    skipSep
    M.void $ satisfy (==close)
    return $ case lastElement of
        Just p -> elements ++ [p]
        Nothing -> elements
    where pElemAndSep = do
            el <- parser
            skipSep
            return el
          skipSep = P.many $ satisfy (==sep)


pEnclosedAndSeparated2 ::
    Show t => 
    Parser ()
    -> Parser ()
    -> Parser ()
    -> Parser t
    -> Parser [t]
pEnclosedAndSeparated2 open close sep pEl = do
    open 
    elements <- P.many $ P.try pElemAndSep 
    lastElement <- P.optionMaybe pEl
    close
    return $ case lastElement of
        Just p -> elements ++ [p]
        Nothing -> elements
    where pElemAndSep = do
            el <- pEl
            sep
            return el



