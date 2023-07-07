module Write (writeMod) where

import qualified Ast
import qualified Data.List as List

writeMod :: Ast.Module -> String
writeMod (Ast.Module _ stmts) = wStmtsTL stmts

wStmts :: [Ast.Stmt] -> String
wStmts = wList "" wStmt

-- For top level statements some things are different. 
-- For instance things need a type, fns cannot be closures, 
-- and vars ned to be const or static.
wStmtsTL :: [Ast.Stmt] -> String
wStmtsTL stmts = wList "" wStmtTL (bundleVarTypesAndAssignments stmts)

wStmtTL :: Ast.Stmt -> String
wStmtTL (Ast.VarTypeDeclAssign (Ast.IdentPattern id) (Ast.FnType pts rty) (Ast.Lambda prms expr)) =
    "pub fn " <> wVarIdent id <> "(" <> psWithTypes <> ") -> " <> wType rty <> " {\n" <> wExpr expr <> "\n}\n\n"
    where psWithTypes = List.intercalate ", " $ zipWith (curry comb) prms pts
          comb (pid, pty) = wPattern pid <> ": " <> wType pty
wStmtTL (Ast.VarTypeDeclAssign (Ast.IdentPattern id) (Ast.SimpleType tid) expr) =
    "pub const " <> wVarIdent id <> ": " <> wTypePath tid <> " = " <> wExpr expr <> ";\n"
wStmtTL Ast.VarAssignment {} = error "No top level type inferring"
wStmtTL Ast.VarTypeDecl {} = error "No top level type declerations without implementation"
wStmtTL stmt = wStmt stmt

bundleVarTypesAndAssignments :: [Ast.Stmt] -> [Ast.Stmt]
bundleVarTypesAndAssignments stmts = rest ++ bundled
    where
        (tys, defs, rest) = splitStmts stmts
        bundled = map bundle $ findVarTypePairs tys defs
        bundle (Ast.VarTypeDecl _ ty, Ast.VarAssignment id @ (Ast.IdentPattern _) expr) =
            Ast.VarTypeDeclAssign id ty expr
        bundle _ = error "Somehow 'splitStmts' does not work"

splitStmts :: [Ast.Stmt] -> ([Ast.Stmt], [Ast.Stmt], [Ast.Stmt])
splitStmts = foldr addToRightList ([], [], [])
    where addToRightList stmt (tys, defs, rest) = case stmt of
            ass @ Ast.VarAssignment {} -> (tys, stmt:defs, rest)
            ty @ (Ast.VarTypeDecl _ _) -> (stmt:tys, defs, rest)
            other -> (tys, defs, stmt:rest)

findVarTypePairs :: [Ast.Stmt] -> [Ast.Stmt] -> [(Ast.Stmt, Ast.Stmt)]
findVarTypePairs tys defs = [ (x, y) | x<-tys, y<-defs, haveSameIdent x y]
    where
        haveSameIdent (Ast.VarTypeDecl id1 _) (Ast.VarAssignment (Ast.IdentPattern id2) _)
            | id1 == id2 = True
            | otherwise = False
        haveSameIdent (Ast.VarTypeDecl id1 _) Ast.VarAssignment {} = False
        haveSameIdent _ _ = error "Lists do not only contain types and assigments"

wStmt :: Ast.Stmt -> String
wStmt (Ast.VarAssignment id expr) = "let " <> wPattern id <> " = " <> wExpr expr <> ";\n"
wStmt (Ast.VarTypeDecl id ty) = "let " <> wVarIdent id <> ": " <> wType ty <> ";\n"
wStmt (Ast.VarTypeDeclAssign id ty expr) =
    "let " <> wPattern id <> wType ty <> " = " <> wExpr expr <> ";\n"
wStmt (Ast.TypeDef id (Ast.Struct stmts)) =
    wDefaultStructAttributes 
    <> "pub struct " <> id <> " {\n" <> wList ",\n" wStructDefStmt stmts <> "\n}\n\n"
    <> if length stmts < 5 then 
        "pub fn " <> id <> "(" <> wList ", " wStructDefStmt stmts <> ") -> " 
        <> id <> " { " <> id <> " { " <> wList ", " wVarName stmts <> " } }\n\n"
    else ""
    where wVarName (Ast.VarTypeDecl id _) = wVarIdent id
wStmt (Ast.TypeDef id (Ast.TupleStruct tys)) =
    wDefaultStructAttributes 
    <> "pub struct " <> id 
    <> "(" <> wList ", " wType tys <> ")\n\n"
wStmt (Ast.TraitDef id stmts) =
    "pub trait " <> id <> " {\n" <> wList "\n" wTraitDefStmt stmts <> "\n}\n\n"
wStmt (Ast.TypeDef id (Ast.Enum ops)) =
    wDefaultStructAttributes <> "pub enum " <> id <> " {\n" <> wList ",\n" wEnumOption ops <> "\n}\n\n"
    <> "pub use " <> id <> "::*;\n\n"
wStmt (Ast.TypeDef id ty) = "pub type " <> id <> " = " <> wType ty <> ";\n\n"
wStmt (Ast.TypeImpl id stmts) = "impl " <> id <> " {\n" <> wStmtsTL stmts <> "}\n"
wStmt (Ast.TraitImpl ty trait stmts) = "impl " <> wTrait trait <> " for " <> ty <> " {\n" <> wStmtsTL stmts <> "}\n\n"

wExpr :: Ast.Expr -> String
wExpr (Ast.Var vp) = wVarPath vp
wExpr (Ast.IntLiteral int) = show int 
wExpr (Ast.StringLiteral str) = "String::from(\"" <> str <> "\")"
wExpr (Ast.FloatLiteral fl) = show fl 
wExpr (Ast.VecLiteral exprs) = "vec![" <> wList ", " wExpr exprs <> "]"
wExpr (Ast.Block stmts expr) =
    "{\n" <> wStmts stmts <> wExpr expr <> "\n}"
wExpr (Ast.FnCall id params) = wVarPath id <> "(" <> wList ", " wExpr params <> ")"
wExpr (Ast.Condition (if', then') elifs else') =
    "if " <> wExpr if' <> " { " <> wExpr then' <> " } " <> elifs' <> "else { " <> wExpr else' <> " }"
    where elifs' = List.intercalate "" $ map str elifs
          str (cond, expr) = "else if " <> wExpr cond <> " { " <> wExpr expr <> " } "
wExpr (Ast.WhereExpr expr stmts) =
    "{\n" <> wStmts stmts <> wExpr expr <> "\n}"
wExpr (Ast.TupleLiteral exprs) = "(" <> wList ", " wExpr exprs <> ")"
wExpr (Ast.BinaryOp ex1 op ex2) = wExpr ex1 <> " " <> Ast.bopToStr op <> " " <> wExpr ex2
wExpr (Ast.EnclosedExpr expr) = "(" <> wExpr expr <> ")"
wExpr (Ast.TupleStructConstructor id exprs) = wTypePath id <> "(" <> wList ", " wExpr exprs <> ")"
wExpr (Ast.AnonStructConstructor (Ast.Block stmts _)) =
    let 
        getId stmt = case stmt of
            Ast.VarTypeDeclAssign (Ast.IdentPattern id) _ _ -> id
            _ -> error "stmt in anonymous struct constructor does not only contain vartypedeclassign with identpattern"
        stmts' = List.sortOn getId stmts
        getType stmt = case stmt of
            Ast.VarTypeDeclAssign (Ast.IdentPattern _) ty _ -> ty
            _ -> error "stmt in anonymous struct constructor does not only contain vartypedeclassign with identpattern"
        tys = map getType stmts'
    in 
    "{\n" 
    <> "#[repr(C)]\n"
    <> "struct X {\n" 
    <> wList ", " wStructDefStmt stmts'
    <> "\n}\n"
    <> "std::mem::transmute::<("
    <> wList ", " wType tys
    <> ")>( X {\n"
    <> wList ", " wStructConstStmt stmts'
    <> "\n} )"
    <> "}\n" 
wExpr (Ast.StructConstructor id stmts unrav) =
    wTypePath id <> " {\n" <> wList ", " wStructConstStmt stmts <> unrav' <> "\n}"
    where getId (Ast.VarAssignment id _) = id
          getId (Ast.VarTypeDeclAssign id _ _) = id
          getId _ = error "Cannot happen, 'dotstmts' not right"
          unrav' = case unrav of
                Just expr -> ",\n.." <> wExpr expr 
                Nothing -> ""
wExpr (Ast.UnaryOp Ast.MakeRef (Ast.VecLiteral exprs)) = "&[" <> wList ", " wExpr exprs <> "]"
wExpr (Ast.UnaryOp Ast.MakeRef (Ast.StringLiteral str)) = "\"" <> str <> "\""
wExpr (Ast.UnaryOp Ast.Negative expr) = "-" <> wExpr expr
wExpr (Ast.UnaryOp Ast.MakeRef expr) = "&" <> wExpr expr
wExpr (Ast.UnaryOp Ast.MakePtr expr) = "Rc::new(" <> wExpr expr <> ")"
wExpr (Ast.LabelConstructor id) = wTypePath id
wExpr (Ast.Match expr arms) = "match " <> wExpr expr <> " {\n" <> wList ",\n" wMatchArm arms <> "\n}"
wExpr (Ast.BooleanMatch expr pat) = "match " <> wExpr expr <> " { " <> wPattern pat <> " => true, _ => false }"
wExpr (Ast.BoolLiteral x) = if x then "true" else "false"
wExpr (Ast.Lambda pats expr) = "&|" <> wList ", " wPattern pats <> "| " <> wExpr expr
wExpr (Ast.DotTrail expr exprs) = wExpr expr <> "\n." <> wList "\n." wExpr exprs 
wExpr (Ast.InterpolatedStringLit (Ast.InterpolatedString strexprs str)) = 
    "format!(\"" <> wList rustPlaceholder fst strexprs <> rustPlaceholder <> str <> "\", " <> wList ", " wVarIdent (map snd strexprs) <> ")"
    where rustPlaceholder = "{#:?}" 

wType :: Ast.Type -> String
wType (Ast.SimpleType id) = wTypePath id
wType (Ast.FnType pts rty) = "&dyn Fn(" <> wList ", " wType pts <> ") -> " <> wType rty
wType (Ast.Tuple tys) = "(" <> wList ", " wType tys <> ")"
wType (Ast.Vec ty) = "Vec<" <> wType ty <> ">"
wType (Ast.Ref (Ast.Vec ty)) = "&[" <> wType ty <> "]"
wType (Ast.Ref (Ast.String)) = "&str"
wType (Ast.Ref ty) = "&" <> wType ty
wType (Ast.Ptr ty) = "Rc<" <> wType ty <> ">"
wType (Ast.TraitObj id) = "&dyn " <> id
wType (Ast.String) = "String"
wType _ = undefined

wVarIdent :: Ast.VarIdent -> String
wVarIdent id = id

wStructConstStmt :: Ast.Stmt -> String
wStructConstStmt (Ast.VarAssignment (Ast.IdentPattern id) expr) = wVarIdent id <> ": " <> wExpr expr 
wStructConstStmt (Ast.VarTypeDeclAssign (Ast.IdentPattern id) _ expr) = wVarIdent id <> ": " <> wExpr expr 

wStructDefStmt :: Ast.Stmt -> String
wStructDefStmt (Ast.VarTypeDecl id ty) = wVarIdent id <> ": " <> wType ty
wStructDefStmt (Ast.VarTypeDeclAssign (Ast.IdentPattern id) ty _) = wVarIdent id <> ": " <> wType ty
wStructDefStmt _ = error "Use the 'ident: Type' syntax in struct definitions"

wTraitDefStmt :: Ast.Stmt -> String
wTraitDefStmt (Ast.VarTypeDecl id (Ast.FnType ptys rty)) =
    "fn " <> wVarIdent id <> "(" <> typesWithNames <> ") -> " <> wType rty <> ";"
    where typesWithNames = List.intercalate ", " (map addIdAndColon $ zip [0..] ptys)
          addIdAndColon (0, Ast.SimpleType (Ast.TypePath Nothing "Self")) = "self"
          addIdAndColon (0, Ast.Ref (Ast.SimpleType (Ast.TypePath Nothing "Self"))) = "&self"
          addIdAndColon (i, ty) = "p" <> show i <> ": " <> wType ty
wTraitDefStmt _ = error "Only add types and fn defs to a trait body"

wStructConstructorStmt :: Ast.Stmt -> String
wStructConstructorStmt (Ast.VarAssignment id expr) = wPattern id <> ": " <> wExpr expr
wStructConstructorStmt _ = error "Use 'attr = value' syntax in constructors"

wEnumOption :: Ast.EnumOption -> String
wEnumOption (Ast.Label id) = id
wEnumOption (Ast.LabeledTuple tys id) = id <> "(" <> wList ", " wType tys <> ")"
wEnumOption (Ast.LabeledStruct stmts id) = id <> " {\n" <> wList ", " wStructDefStmt stmts <> "\n}"

wMatchArm :: (Ast.Pattern, Ast.Expr) -> String
wMatchArm (pat, expr) = wPattern pat <> " => " <> wExpr expr

wPattern :: Ast.Pattern -> String
wPattern (Ast.IntLitPattern int) = show int
wPattern (Ast.FloatLitPattern flt) = show flt
wPattern (Ast.StringLitPattern str) = "\"" <> str <> "\""
wPattern (Ast.ArrayDestr ptns) = "[" <> wList ", " wPattern ptns <> "]"
wPattern (Ast.TupleDestr ptns) = "(" <> wList ", " wPattern ptns <> ")"
wPattern Ast.DotDotPattern = ".."
wPattern Ast.WildCardPattern = "_"
wPattern (Ast.IdentPattern id) = wVarIdent id
wPattern (Ast.LabelPattern id) = id
wPattern (Ast.TupleStructDestr id ptns) = id <> "(" <> wList ", " wPattern ptns <> ")"
wPattern (Ast.StructDestr id ptns) = id <> "{\n" <> wList ", " wPattern ptns <> "\n}"
wPattern (Ast.IdValuePattern id pat) = wVarIdent id <> ": " <> wPattern pat
wPattern (Ast.CatchPattern id pat) = wVarIdent id <> " @ " <> wPattern pat
wPattern (Ast.BooleanLitPattern bool) = if bool then "true" else "false"
wPattern (Ast.OrPattern pats) = wList "\n| " wPattern pats
wPattern (Ast.Guarded pat expr) = wPattern pat <> " if " <> wExpr expr

wTrait trait = trait 

wVarPath :: Ast.VarPath -> String
wVarPath (Ast.VarPath mod id) = case mod of  
        Just m -> m <> "::" <> id
        Nothing -> id

wTypePath :: Ast.TypePath -> String
wTypePath (Ast.TypePath mod id) = case mod of
        Just m -> m <> "::" <> id
        Nothing -> id

-- UTILS

wList ::  String -> (t -> String) -> [t] -> String
wList sep writer lst = List.intercalate sep (map writer lst)

wDefaultStructAttributes :: String
wDefaultStructAttributes = "#[derive(Clone, Copy, Debug)]\n"
