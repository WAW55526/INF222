module CheckAliasing where

import BIPL3AST
import PIPLMeta
import BIPL3MetaTypes

-- | Check if a program has any aliasing issues (i.e. if any procedure calls
-- | appearing in the program try to pass the same variable as multiple output
-- | parameters at the same time).
--
-- | Returns False if issues are found.
checkAliasing :: Program -> Bool
checkAliasing [] = True
checkAliasing (x:xs) = if checkAliasing' x 
                        then checkAliasing xs 
                        else False

checkAliasing' :: ProcedureDeclaration -> Bool
checkAliasing' (Proc _ params _ stmt) = noAlias
    where
        varExps = findVarExprs stmt []
        duplicates = filterDuplicates varExps
        outParams = filterParams params
        noAlias = checkParams duplicates outParams

checkParams :: [Var] -> [Parameter] -> Bool
checkParams [] _ = True
checkParams (v:vs) params = if checkParams' v params
                                then checkParams vs params
                                else False

checkParams' :: Var -> [Parameter] -> Bool
checkParams' var [] = True
checkParams' var ((mode,(name, t)):ps) = if var == name
                                            then False
                                            else checkParams' var ps

findVarExprs :: Stmt -> [Var] -> [Var]
findVarExprs (While _ stmt) varExps = findVarExprs stmt varExps
findVarExprs (IfStmt _ stmt1 stmt2) varExps = findVarExprs stmt2 (findVarExprs stmt1 varExps)
findVarExprs (Sequence []) varExps = varExps
findVarExprs (Sequence (s:ss)) varExps = findVarExprs (Sequence ss) (findVarExprs s varExps)
findVarExprs (CallProc _ []) varExps = varExps
findVarExprs (CallProc name (e:es)) varExps = findVarExprs (CallProc name es) (findVarExprs' e varExps)
findVarExprs _ varExps = varExps

findVarExprs' :: Expr -> [Var] -> [Var]
findVarExprs' (CallFun _ []) varExps = varExps
findVarExprs' (CallFun name (e:es)) varExps = findVarExprs' (CallFun name es) (findVarExprs' e varExps)
findVarExprs' (VarExp var) varExps = var : varExps
findVarExprs' _ varExps = varExps

filterDuplicates :: [Var] -> [Var]
filterDuplicates [] = []
filterDuplicates (x:xs) = if elem x xs
                            then x : filterDuplicates xs
                            else filterDuplicates xs

filterParams :: [Parameter] -> [Parameter]
filterParams [] = []
filterParams ((mode,varDecl):xs) = if mode == Obs
                                    then filterParams xs
                                    else (mode,varDecl) : filterParams xs
