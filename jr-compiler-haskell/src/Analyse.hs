

module Analyse where

-- import qualified Ast
-- import qualified Data.Set as Set

-- newtype Context = Context { varNames :: Set.Set String }

-- analyse :: Ast.Module -> Ast.Module
-- analyse = uModule (Context Set.empty) 

-- -- UPDATE

-- uModule :: Context -> Ast.Module -> Ast.Module
-- uModule ctx (Ast.Module head stmts) = Ast.Module head (uStmts ctx stmts)

-- uStmts :: Context -> [Ast.Stmt] -> [Ast.Stmt]
-- uStmts ctx stmts = map (uStmt ctx') stmts
--     where
--         ctx' = foldl updateContext ctx stmts
--         updateContext oldctx stmt = case stmt of
--             Ast.VarAssignment pat _ -> addVarsToContext oldctx (getVarsFromPattern pat)
--             Ast.VarTypeDeclAssign pat _ _ -> addVarsToContext oldctx (getVarsFromPattern pat) 
--             Ast.VarTypeDecl id _ -> addVarsToContext oldctx [id]

-- uStmt :: Context -> Ast.Stmt -> Ast.Stmt
-- uStmt ctx (Ast.VarAssignment pat expr) = Ast.VarAssignment pat (uExpr ctx expr)
-- uStmt ctx (Ast.VarTypeDeclAssign pat ty expr) = Ast.VarTypeDeclAssign pat ty (uExpr ctx expr)
-- uStmt ctx (Ast.TypeImpl ty stmts) = Ast.TypeImpl ty (uStmts ctx stmts)
-- uStmt ctx (Ast.TraitImpl ty trait stmts) = Ast.TraitImpl ty trait (uStmts ctx stmts)
-- uStmt ctx stmt = stmt

-- uExpr :: Context -> Ast.Expr -> Ast.Expr
-- uExpr = uExprModOrVar

-- uExprs :: Context -> [Ast.Expr] -> [Ast.Expr]
-- uExprs ctx = map (uExpr ctx)

-- -- UPDATE TASKS

-- -- hai.hai2() could mean two things, that hai is a mod and hai2 a function
-- -- or that hai is a var and hai a method.

-- -- WALK EXPRESSION
-- walkExpr :: 
--     (Ast.Expr -> Maybe Ast.Expr) 
--     -> Context 
--     -> Ast.Expr 
--     -> Ast.Expr
-- walkExpr ufn ctx expr = case expr of
--     Ast.VecLiteral exprs -> Ast.VecLiteral (uExprs' exprs)
--     Ast.TupleLiteral exprs -> Ast.TupleLiteral (uExprs' exprs)
--     Ast.Lambda pats expr -> Ast.Lambda pats (uExpr' expr)
--     Ast.BinaryOp expr op expr1 -> Ast.BinaryOp (uExpr ctx expr) op (uExpr' expr1)
--     Ast.UnaryOp op expr -> Ast.UnaryOp op (uExpr' expr)
--     Ast.FnCall id exprs -> Ast.FnCall id (uExprs' exprs)
--     Ast.EnclosedExpr expr -> Ast.EnclosedExpr (uExpr' expr)
--     Ast.Condition (ife, thene) elifs ele -> Ast.Condition (uExpr' ife, uExpr' thene) elifs' (uExpr' ele)
--         where elifs' = map (\(elif, thene) -> (uExpr' elif, uExpr' thene)) elifs
--     Ast.
--     expr -> expr
--     where uExpr' = uExpr ctx
--           uExprs' = uExprs ctx

-- -- UTILS

-- addVarsToContext :: Context -> [String] -> Context
-- addVarsToContext ctx = foldl addVar ctx 
--     where addVar oldctx var = oldctx { varNames = Set.insert var (varNames oldctx) }

-- getVarsFromPattern :: Ast.Pattern -> [String]
-- getVarsFromPattern pat = case pat of
--     Ast.TupleDestr pats -> getVarsFromPatterns pats
--     Ast.ArrayDestr pats -> getVarsFromPatterns pats
--     Ast.StructDestr _ pats -> getVarsFromPatterns pats
--     Ast.TupleStructDestr _ pats -> getVarsFromPatterns pats
--     Ast.CatchPattern id pat -> id : getVarsFromPattern pat
--     Ast.IdValuePattern id pat -> getVarsFromPattern pat
--     Ast.OrPattern pats -> getVarsFromPatterns pats
--     Ast.Guarded pat _ -> getVarsFromPattern pat
--     Ast.IdentPattern id -> [id]
--     _ -> []


-- getVarsFromPatterns :: [Ast.Pattern] -> [String]
-- getVarsFromPatterns = foldl addPattern [] 
--     where addPattern lst pat = lst ++ getVarsFromPattern pat 


    