-- Task 8.2.4
-- Author William A. Winterstø
-- Since 29.04.2022

module Pam8UTypedInterpreter where

-- Use ASTs based on signatures.
import Pam8Signature
import Pam8UTypedSignatureAST

-- Use state for variables with store.
import Pam8State


-----------------------
-- | Evaluate a Calculator expression with a function model and state.
-- The function model of the intrinsic functions are given by the funmod argument.
evaluate :: FunModel valuedomain -> State valuedomain -> TCalcExprAST valuedomain -> valuedomain
evaluate funmod state (TLit i _ _) = i
evaluate funmod state (TFun str args _) 
  = funmod str (map (evaluate funmod state) args)
evaluate funmod state (TVar vname _) = getValue vname state

-- | Execute a statement: set/assign a variable in state from a calculator expression.
execute :: Show valuedomain => FunModel valuedomain -> TCalcStmtAST valuedomain -> State valuedomain -> State valuedomain
execute funmod (TSetVar vname exp) state
  = addVariable vname (evaluate funmod state exp) state
execute funmod (TAssVar vname exp) state
  = changeValue vname (evaluate funmod state exp) state


-----------------------
-- | Check if all variables in an expression have been declared in the state.
-- Returns a list of all undeclared variables.
allDeclared :: State valuedomain -> TCalcExprAST valuedomain -> [VarName]
allDeclared state (TLit _ _ _) = []
allDeclared state (TFun fname args _) = foldl (++) [] (map (allDeclared state) args)
allDeclared state (TVar vname _) = if isDeclared vname state then [] else [vname]


-----------------------
-- | Unit test for calculator with variables and open ended set of intrinsic functions.
-- Can only test the structural part of the calculator:
-- declaring, assigning and accessing variables using "fake" semantic function testfunmod.
unittestPam8UTypedInterpreter = do
  print $ "-- unittestPam8UTypedInterpreter --"
  let -- | Fake integer semantics function: always returns 5.
      testfunmod :: FunModel Integer
      testfunmod str plist = 5
  -- Create variables in order, but with the wrong values.
  let state1 = execute testfunmod (TSetVar "x" (TLit 11 "" "")) newState
  let state2 = execute testfunmod (TSetVar "y" (TLit 10 "" "")) state1
  let state3 = execute testfunmod (TSetVar "z" (TLit 23 "" "")) state2
  -- Correct the values for the variables out of order.
  let state4 = execute testfunmod (TAssVar "y" (TLit 37 "" "")) state3
  let state5 = execute testfunmod (TAssVar "z" (TLit 39 "" "")) state4
  let state6 = execute testfunmod (TAssVar "x" (TLit 31 "" "")) state5
  -- Create the expected set of variable-values.
  let state1' = execute testfunmod (TSetVar "x" (TLit 31 "" "")) newState
  let state2' = execute testfunmod (TSetVar "y" (TLit 37 "" "")) state1'
  let state3' = execute testfunmod (TSetVar "z" (TLit 39 "" "")) state2'
  -- Checking if all variables in an AST have been declared.
  let expr = TFun "Add" [(TVar "x" ""), (TVar "z" "")] ""
  let chv0 = allDeclared newState expr == ["x","z"]
  let chv1 = allDeclared state1 expr == ["z"]
  let chv2 = allDeclared state3 expr == []
  -- Output result of unit test
  putStrLn $ 
    if (state3' == state6)
    && (evaluate testfunmod state6 (TVar "x" "") == 31)
    && (evaluate testfunmod state6 (TVar "y" "")  == 37)
    && (evaluate testfunmod state6 (TVar "z" "")  == 39)
    && (testfunmod "func" []) == 5
    && chv0 && chv1 && chv2
    then "Unit tests hold"
    else "Tests failed"
