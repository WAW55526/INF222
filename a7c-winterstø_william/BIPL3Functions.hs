module BIPL3Functions where

import BIPL3State
import PIPLInterpreter (FunModel)
import BIPL3MetaTypes
import PIPLMeta

import Data.List (intersperse)

biplFunModel :: FunModel
biplFunModel "plus" [I x, I y] = I (x + y)
biplFunModel "mult" [I x, I y] = I (x * y)
biplFunModel "equal" [I x, I y] = B (x == y)
biplFunModel "le" [I x, I y] = B (x <= y)
biplFunModel "gt" [I x, I y] = B (x > y)
biplFunModel "uminus" [I x] = I (-x)
biplFunModel "and" [B x, B y] = B (x && y)
biplFunModel "or" [B x, B y] = B (x || y)
biplFunModel "not" [B x] = B (not x)
biplFunModel fName args =
    error $ "Function call " ++ fName ++ "(" ++ commaList (map show args) ++
        ") can not be evaluated in this model."
            where commaList = concat . intersperse ", "

biplFunSigs :: FunSigs
biplFunSigs = [ ("plus", ([IntegerType, IntegerType], IntegerType))
              , ("mult", ([IntegerType, IntegerType], IntegerType))
              , ("equal", ([IntegerType, IntegerType], BoolType))
              , ("le", ([IntegerType, IntegerType], BoolType))
              , ("gt", ([IntegerType, IntegerType], BoolType))
              , ("uminus", ([IntegerType], IntegerType))
              , ("and", ([BoolType, BoolType], BoolType))
              , ("or", ([BoolType, BoolType], BoolType))
              , ("not", ([BoolType], BoolType))
              ]


