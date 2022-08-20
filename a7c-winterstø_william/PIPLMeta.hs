module PIPLMeta where

import BIPL3MetaTypes
import BIPL3AST

-- INFO: This declaration is semantically equivalent to the old one:
--
-- data ProcedureDeclaration = Proc ProcName [Parameter] [VarDecl] Stmt
--
-- except it also defines 4 functions you can use on a ProcedureDeclaration to
-- extract each of the 4 fields.
-- I.e. you can still pattern match on `(Proc name params locals stmt)` as before,
-- or if you have a variable `p` which is a ProcedureDeclaration, you can use
-- `procName p`, `procParams p`, etc. to extract the value of a field.
data ProcedureDeclaration =
    Proc { procName :: ProcName
         , procParams :: [Parameter]
         , procLocals :: [VarDecl]
         , procBody :: Stmt
         }
    deriving (Show, Eq, Read)

-- | A program is simply a list of procedures.
type Program = [ProcedureDeclaration]

data Mode = Obs | Upd | Out
    deriving (Show, Eq, Read)

-- | Procedure parameters: mode and variable declaration
type Parameter = (Mode, VarDecl)

paramMode :: Parameter -> Mode
paramMode = fst

paramVar :: Parameter -> VarDecl
paramVar = snd

getProc :: ProcName -> Program -> ProcedureDeclaration
getProc pName programs = case filter (\proc -> procName proc == pName) programs of
                           (p:_) -> p
                           [] -> error $ "getProc: Procedure " ++ pName ++ " not found"
