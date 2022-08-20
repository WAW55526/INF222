-- | AST for variable based value domain calculator with explicit documented signatures.
--
-- Author Magne Haveraaen
-- Since 2020-03-19

module Pam8SignatureAST where

-- Based on signatures and function models
import Pam8Signature

-----------------------
-- | Expressions for a calculator with variables.
-- The calculator supports literals Lit for any value domain,
-- an open ended set of primitive functions Fun, and
-- an open ended set of variables Var.
data CalcExprAST valuedomain
  = Lit valuedomain
  | Fun FunName [CalcExprAST valuedomain]
  | Var VarName
  deriving (Eq, Read, Show)


-- | Statement for declaring (setting) and changing (assigning) a variable
data CalcStmtAST valuedomain
  = SetVar VarName (CalcExprAST valuedomain)
  | AssVar VarName (CalcExprAST valuedomain)
  deriving (Eq, Read, Show)

-----------------------
-- | Check that an expression is compatible with a given signature.
-- Returns a list of undeclared functions used in the expression.
typeCheckExpr :: Signature -> CalcExprAST valuedomain -> [FunName]
typeCheckExpr sig@(types,fundecls) (Lit _) = []
typeCheckExpr sig@(types,fundecls) fcall@(Fun fn exprs) = typeCheckExpr' fundecls fcall
  where
    typeCheckExpr' :: [FunDeclaration] -> CalcExprAST valuedomain -> [FunName]
    typeCheckExpr' ((fname,params,res,doc):fundecls) fcall@(Fun fn exprs)
      = if fname == fn && length params == length exprs
        then undeclared exprs
        else typeCheckExpr' fundecls fcall
    typeCheckExpr' [] fcall@(Fun fn exprs) = fn:undeclared exprs
    -- | Check subexpressions
    undeclared exprs = foldr (++) [] (map (typeCheckExpr sig) exprs)
typeCheckExpr sig@(types,fundecls) (Var _) = []

-----------------------
-- | Unit test for typeCheckExpr
-- Contains an example of an AST and several related signatures.
unittestPam8SignatureAST = do
  print $ "-- unittestPam8SignatureAST --"
  let sig1 = ([("Int","")],[("Neg",["Int"],"Int","")])::Signature
  let sig2 = ([("Int","")],[("Neg",["Int"],"Int",""),("Add",["Int"],"Int","")])::Signature
  let sig3 = ([("Int","")],[("Neg",["Int"],"Int",""),("Add",["Int","int"],"Int","")])::Signature
  let sig4 = ([("Int","")],[("Neg",["Int"],"Int",""),("Add",["Int","int"],"Int",""),("Mult",["Int","int"],"Int",""),("Sub",["Int","int"],"Int","")])::Signature
  let expr = Fun "Neg" [Fun "Mult" [Fun "Add" [(Lit 3),(Fun "Sub" [(Lit 7),(Lit 13)])],(Lit 19)]]
  let ch1 = typeCheckExpr sig1 expr == ["Mult","Add","Sub"]
  let ch2 = typeCheckExpr sig2 expr == ["Mult","Add","Sub"]
  let ch3 = typeCheckExpr sig3 expr == ["Mult","Sub"]
  let ch4 = typeCheckExpr sig4 expr == []
  print $ if ch1 && ch2 && ch3 && ch4 then "OK" else "Not OK"

