module BIPL3MetaTypes where

-- | Variables are strings
type Var = String

-- | Function names are strings
type FunName = String

-- | Procedure names are strings
type ProcName = String

data Type = IntegerType | BoolType | LocationType
    deriving (Show, Eq, Read)

type FunSig = ([Type], Type)
--               ^       ^--- return type
--               |
--               \---- Argument types

type FunSigs = [(FunName, FunSig)]

getFunSig  :: FunName -> FunSigs -> FunSig
getFunSig funName funSigs =
    case lookup funName funSigs of
      Nothing -> error $ "getFunSig: Function " ++ funName ++ " not found."
      Just f -> f

-- | Variable declaration: variable name and its type
type VarDecl = (Var, Type)

varName :: VarDecl -> Var
varName = fst

varType :: VarDecl -> Type
varType = snd

type VarDecls = [VarDecl]

getVarType :: Var -> VarDecls -> Type
getVarType varName decls =
    case lookup varName decls of
      Nothing -> error $ "getVarType: Variable " ++ varName ++ " not found."
      Just t -> t

