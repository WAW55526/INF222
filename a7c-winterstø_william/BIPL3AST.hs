-- | BIPL3AST: Abstract syntax for BIPL,
-- BTL3 extended with statements (including Assert)
-- @date 2022-02-10
-- @author Magne Haveraaen

module BIPL3AST where

import BIPL3MetaTypes

-- | Expression abstract syntax: BTL with Integers and booleans
data Expr =
    IL Integer -- integer literal
    | BL Bool -- boolean literal
    | VarExp Var -- a variable is an expression
    | CallFun FunName [Expr]
    deriving (Show, Eq, Read)

-- | Statement abstract syntax: from L0602
data Stmt =
    Assert Expr
    | Assign Var Expr
    | While Expr Stmt
    | IfStmt Expr Stmt Stmt
    | Sequence [Stmt]
    | CallProc ProcName [Expr]
    deriving (Show, Eq, Read)
