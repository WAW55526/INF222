module CheckExprType where

import BIPL3AST
import PIPLMeta
import BIPL3State
import BIPL3MetaTypes

-- | Check if an expression is correctly typed, and if so, return its type.
-- | If it's not correctly typed, use `error` to throw an exception with an
-- | appropriate error message.
--
-- | To be correctly typed, a FunCall has to
-- | * Call a function that exists (i.e. that has an entry in the provided FunSigs).
-- | * Pass arguments that have the same types as defined in the signature of
-- |   the function being called.
-- |   E.g. for the expression `FunCall "not" args`
-- |   if we're using biplFunSigs from BIPL3Functions.hs,
-- |   then `args` should be a list containing a single expression whose type 
-- |   is `BoolType`, otherwise the FunCall is malformed.
--
-- | The (VarDecls, FunSigs) parameter provides a context for type checking.
-- | E.g. we can't know the type of `VarExp "x"` in a vacuum -- variable types
-- | need to be provided somehow. Likewise, we can't check if CallFun
-- | expressions are well-typed in a vacuum -- the signatures of defined
-- | functions need to be provided. (You don't have to provide them here -- they
-- | should be provided by whoever calls checkType).
checkType :: Expr -> (VarDecls, FunSigs) -> Type
checkType (IL i) (varTypes, funSigs) = IntegerType
checkType (BL b) (varTypes, funSigs) = BoolType
checkType (VarExp var) (varTypes, funSigs) = getVarType var varTypes
checkType (CallFun funName args) (varTypes, funSigs) = result
    where
        sig = getFunSig funName funSigs
        types = fst sig
        correctSig = checkArgTypes args types (varTypes, funSigs)
        result = if correctSig
                    then snd sig
                    else error "Incorrect function signature"

checkArgTypes :: [Expr] -> [Type] -> (VarDecls, FunSigs) -> Bool
checkArgTypes [] [] _ = True
checkArgTypes _ [] _ = False
checkArgTypes [] _ _ = False
checkArgTypes (x:xs) (y:ys) varfun = if checkType x varfun == y
                                        then checkArgTypes xs ys varfun
                                        else False
