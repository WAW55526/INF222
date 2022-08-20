-- | BIPL4TypedAST: Abstract syntax for BIPL3 with type annotations.
-- Typing is very permissive - failing only on a few pathological cases.
-- @date 2022-02-16
-- @author Magne Haveraaen

-- Remember option -Wincomplete-patterns to the Haskell compiler.
module BIPL4TypedAST where

-- Plain AST for BIPL with assert.
import BIPL3AST

-----------------------
-- | Intrinsic types
data Type = Unknown | IntegerType | BoolType | LocationType
    deriving (Show, Eq, Read)

-----------------------

-- | TExpression abstract syntax: BTL with Integers and booleans
data TExpr = 
    TIL Integer -- integer literal
    | TPlus TExpr TExpr Type
    | TMult TExpr TExpr Type
    | TUminus TExpr Type
    | TDiv TExpr TExpr Type
    | TMod TExpr TExpr Type
    | TBL Bool -- boolean literal
    | TOr TExpr TExpr Type
    | TAnd TExpr TExpr Type
    | TNot TExpr Type
    | TChoice TExpr TExpr TExpr Type
    | TEqual TExpr TExpr Type -- equality between integers
    | TLe TExpr TExpr Type -- less or equal ≤
    | TVarExp Var Type -- a variable is an typed Expression
    deriving (Show, Eq, Read)

-- | Statement abstract syntax: from L0602
data TStmt = 
    TAssert TExpr Type
    | TAssign Var TExpr
    | TWhile TExpr TStmt
    | TIfStmt TExpr TStmt TStmt
    | TSequence [ TStmt ]
    deriving (Show, Eq, Read)

-----------------------

-- | Recover the type from a typed expression
getType :: TExpr -> Type
getType (TIL i) = IntegerType
getType (TPlus te1 te2 typ) = typ
getType (TMult te1 te2 typ) = typ
getType (TUminus te1 typ) = typ
getType (TBL b) = BoolType
getType (TOr te1 te2 typ) = typ
getType (TAnd te1 te2 typ) = typ
getType (TNot te1 typ) = typ
getType (TChoice te0 te1 te2 typ) = typ
getType (TEqual te1 te2 typ) = typ
getType (TLe te1 te2 typ) = typ
getType (TVarExp vname typ) = typ

-- | infer typed expressions from untyped expressions and type environment.
inferTypedExpr :: Expr -> TypeEnvironment -> TExpr
inferTypedExpr (IL i) tenv = TIL i
inferTypedExpr expr@(Plus e1 e2) tenv = TPlus te1 te2 typ where
  te1 = inferTypedExpr e1 tenv
  te2 = inferTypedExpr e2 tenv
  typ1 = getType te1
  typ2 = getType te2
  typ = if typ1 == typ2 && typ1 == IntegerType then typ2 
    else error $ "Inferred type for arguments are not integers: " ++ show (TPlus te1 te2 Unknown)
inferTypedExpr expr@(Mult e1 e2) tenv = TMult te1 te2 typ where
  te1 = inferTypedExpr e1 tenv
  te2 = inferTypedExpr e2 tenv
  typ1 = getType te1
  typ2 = getType te2
  typ = if typ1 == typ2 && typ1 == IntegerType then typ2 
    else error $ "Inferred type for arguments are not integers: " ++ show (TMult te1 te2 Unknown)
inferTypedExpr expr@(Uminus e1) tenv = TUminus te1 typ where
  te1 = inferTypedExpr e1 tenv
  typ1 = getType te1
  typ = if typ1 == IntegerType then typ1
    else error $ "Inferred type for arguments is not integer: " ++ show (TUminus te1 Unknown)
inferTypedExpr expr@(Div e1 e2) tenv = TDiv te1 te2 typ where
  te1 = inferTypedExpr e1 tenv
  te2 = inferTypedExpr e2 tenv
  typ1 = getType te1
  typ2 = getType te2
  typ = if typ1 == typ2 && typ1 == IntegerType then typ2 
    else error $ "Inferred type for arguments are not integers: " ++ show (TPlus te1 te2 Unknown)
inferTypedExpr expr@(Mod e1 e2) tenv = TMod te1 te2 typ where
  te1 = inferTypedExpr e1 tenv
  te2 = inferTypedExpr e2 tenv
  typ1 = getType te1
  typ2 = getType te2
  typ = if typ1 == typ2 && typ1 == IntegerType then typ2 
    else error $ "Inferred type for arguments are not integers: " ++ show (TPlus te1 te2 Unknown)
inferTypedExpr (BL b) tenv = TBL b
inferTypedExpr expr@(Or e1 e2) tenv = TOr te1 te2 typ where
  te1 = inferTypedExpr e1 tenv
  te2 = inferTypedExpr e2 tenv
  typ1 = getType te1
  typ2 = getType te2
  typ = if typ1 == typ2 && typ1 == BoolType then typ2 
    else error $ "Inferred type for arguments are not bools: " ++ show (TOr te1 te2 Unknown)
inferTypedExpr expr@(And e1 e2) tenv = TAnd te1 te2 typ where
  te1 = inferTypedExpr e1 tenv
  te2 = inferTypedExpr e2 tenv
  typ1 = getType te1
  typ2 = getType te2
  typ = if typ1 == typ2 && typ1 == BoolType then typ2 
    else error $ "Inferred type for arguments are not bools: " ++ show (TAnd te1 te2 Unknown)
inferTypedExpr expr@(Not e1) tenv = TNot te1 typ where
  te1 = inferTypedExpr e1 tenv
  typ1 = getType te1
  typ = if typ1 == BoolType then typ1
    else error $ "Inferred type for arguments is not bool: " ++ show (TNot te1 Unknown)
inferTypedExpr expr@(Choice e0 e1 e2) tenv = TMult te1 te2 typ where
  te0 = inferTypedExpr e1 tenv
  te1 = inferTypedExpr e1 tenv
  te2 = inferTypedExpr e2 tenv
  typ0 = getType te0
  typ1 = getType te1
  typ2 = getType te2
  typ = if typ0 == BoolType && typ1 == typ2 then typ2 
    else error $ "Inferred type for arguments do not match: " ++ show (TChoice te0 te1 te2 Unknown)
inferTypedExpr expr@(Equal e1 e2) tenv = TEqual te1 te2 typ where
  te1 = inferTypedExpr e1 tenv
  te2 = inferTypedExpr e2 tenv
  typ1 = getType te1
  typ2 = getType te2
  typ = if typ1 == typ2 && typ1 == IntegerType then BoolType 
    else error $ "Inferred type for comparison arguments do not match: " ++ show (TEqual te1 te2 Unknown)
inferTypedExpr expr@(Le e1 e2) tenv = TLe te1 te2 typ where
  te1 = inferTypedExpr e1 tenv
  te2 = inferTypedExpr e2 tenv
  typ1 = getType te1
  typ2 = getType te2
  typ = if typ1 == typ2 && typ1 == IntegerType then BoolType 
    else error $ "Inferred type for comparison arguments are not integers: " ++ show (TLe te1 te2 Unknown)
inferTypedExpr expr@(VarExp vname) tenv = TVarExp vname (getVariableType vname tenv)


-- | Infer typed statements from statements and type environment
inferTypedStmt :: Stmt -> TypeEnvironment -> (TypeEnvironment,TStmt)
inferTypedStmt (Assert expr) tenv = (tenv,TAssert texpr typ)
  where
  texpr = inferTypedExpr expr tenv
  typ = if getType texpr == BoolType then BoolType 
    else error $ "Inferred type for assertion is not Bool: (" ++ show texpr ++ ")"
inferTypedStmt (Assign var expr) tenv = (tenv',TAssign var texpr) where
  texpr = inferTypedExpr expr tenv
  typ = getType texpr
  -- Add new type declaration if different from current active declaration
  tenv' = if lookup var tenv == Just typ then tenv else (var,typ):tenv
inferTypedStmt wstmt@(While expr stmt) tenv = (tenv'',TWhile texpr tstmt) where
  texpr = inferTypedExpr expr tenv
  (tenv',tstmt) = inferTypedStmt stmt tenv
  tenv'' = if getType texpr == BoolType then tenv'
    else error $ "Inferred type for while conditon is not bool: (" ++ show (TWhile texpr tstmt) ++ ")"
inferTypedStmt ifstmt@(IfStmt expr stmt1 stmt2) tenv = (tenv',TIfStmt texpr tstmt1 tstmt2) where
  texpr = inferTypedExpr expr tenv
  (tenv1,tstmt1) = inferTypedStmt stmt1 tenv
  (tenv2,tstmt2) = inferTypedStmt stmt2 tenv
  tenv' = if getType texpr == BoolType then
    if tenv1 == tenv2 then tenv2
      else error $ "Inferred environments for if branches are different: (" ++ show (TIfStmt texpr tstmt1 tstmt2) ++ ")"
        ++", tenv1 = (" ++ show tenv1 ++ ")" ++ ", tenv2 = (" ++ show tenv2 ++ ")"
    else error $ "Inferred type for if conditon is not bool: (" ++ show (TIfStmt texpr tstmt1 tstmt2) ++ ")"
inferTypedStmt seq@(Sequence (stmt:stmts)) tenv = (tenv'', TSequence (tstmt:tstmts)) where
  (tenv',tstmt) = inferTypedStmt stmt tenv
  (tenv'',TSequence tstmts) = inferTypedStmt (Sequence stmts) tenv'
inferTypedStmt seq@(Sequence []) tenv = (tenv, TSequence [])


-----------------------
-- | A type environemnt for variables.
-- It stores an association list of variable names and their declared type.
type TypeEnvironment = [(Var,Type)]

-- | Defines an empty type environment
emptyTypeEnvironment :: TypeEnvironment
emptyTypeEnvironment = []

-- | Add a new variable (and a store index) to the environment.
addVariableToTypeEnvironment :: String -> Type -> TypeEnvironment -> TypeEnvironment
addVariableToTypeEnvironment vname typ tenv = (vname,typ):tenv

getVariableType :: Var -> TypeEnvironment -> Type
getVariableType vname tenv =
  case lookup vname tenv of
    Just typ -> typ
    Nothing ->  error $ "Variable " ++ vname ++ " not declared in " ++ (show tenv)


-----------------------
-- | Procedure for Euclidean division. 
-- procedure eucliddiv ( obs x,y:integer, out q,r:integer );
eucliddiv_TStmt :: (TypeEnvironment, TStmt)
eucliddiv_TStmt = inferTypedStmt eucliddiv_stmt 
  [("x",IntegerType),("y",IntegerType),("q",IntegerType),("r",IntegerType)]
eucliddiv_tstmt :: TStmt
eucliddiv_tstmt = tstmt where (tenv,tstmt) = eucliddiv_TStmt

--------------------------
-- | Computus
--     procedure computus ( obs Y:integer; out month,day:integer ) ;
computus_TStmt :: (TypeEnvironment, TStmt)
computus_TStmt = inferTypedStmt computus_stmt 
  [("Y",IntegerType),("month",IntegerType),("day",IntegerType)]
computus_tstmt :: TStmt
computus_tstmt = tstmt where (tenv,tstmt) = computus_TStmt

--------------------------
-- | Weirdly typed statements.
wts = [ wtsex1, wtsex1', wtsex1'', wtsex2, wtsex3 True, wtsex3 False]
wtsex1 :: Stmt
wtsex1 = Sequence [ Assign "X" (IL 4), Assign "X" (BL True)]
wtsex1' :: Stmt
wtsex1' = Sequence [ Assign "X" (IL 4), Assign "X" (BL True), Assign "X" (BL False)]
wtsex1'' :: Stmt
wtsex1'' = Sequence [ Assign "X" (IL 4), Assign "X" (BL True), Assign "X" (IL 42)]
wtsex2 :: Stmt
wtsex2 = Sequence [ Assign "X" (IL 4), Sequence [Assign "X" (BL True)] ]
wtsex3 :: Bool -> Stmt
wtsex3 cond = IfStmt (BL cond) (Assign "X" (IL 4)) (Assign "X" (BL True))

-- | Show the type environment and typed statement inferred from an untyped statement.
showWts :: Stmt -> String
showWts wts = show (inferTypedStmt wts []) ++ "\n"

--------------------------
-- | Unit test for type conversion
unittestTypeInference = do
  print $ "Type annotated Euclidean division " ++ show eucliddiv_TStmt
  print $ "Type annotated Computus " ++ show computus_TStmt
  putStrLn ""
  putStrLn $ foldl (++) "" (map showWts wts)
  putStrLn $ show $ map showWts wts
