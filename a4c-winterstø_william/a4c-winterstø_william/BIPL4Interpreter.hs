-- | BIPL4Interpreter: big step operational semantics for typed BIPL (BIPL4), 
-- using environment and store for the state.
-- A permissive semantics with implicit declarations and relaxed typing.
-- @date 2022-02-16
-- @author Magne Haveraaen

module BIPL4Interpreter where

import BIPL3AST

import BIPL4TypedAST

import BIPL3State

-----------------------

-- | Evaluate expressions
eval :: TExpr -> State -> Value
eval (TIL i) state = I (fromIntegral i)
eval (TPlus e1 e2 typ) state = I (e1' + e2') where
  I e1' = eval e1 state
  I e2' = eval e2 state
eval (TMult e1 e2 typ) state = I (e1' * e2') where
  I e1' = eval e1 state
  I e2' = eval e2 state
eval (TUminus e1 typ) state = I (- e1') where
  I e1' = eval e1 state
eval (TBL b) state= B b
eval (TOr e1 e2 typ) state = B (e1' || e2') where
  B e1' = eval e1 state
  B e2' = eval e2 state
eval (TAnd e1 e2 typ) state = B (e1' && e2') where
  B e1' = eval e1 state
  B e2' = eval e2 state
eval (TNot e1 typ) state = B (not e1') where
  B e1' = eval e1 state
eval (TChoice e0 e1 e2 typ) state = (if e0' then e1' else e2') where
  B e0' = eval e0 state
  e1' = eval e1 state
  e2' = eval e2 state
eval (TEqual e1 e2 typ) state = B (e1' == e2') where
  I e1' = eval e1 state
  I e2' = eval e2 state
eval (TLe e1 e2 typ) state = B (e1' <= e2') where
  I e1' = eval e1 state
  I e2' = eval e2 state
eval (TVarExp var typ) state = getValue var state
-- eval exp state = error $ "No such expression " ++ (show exp)


-- | Execute statements
exec :: TStmt -> State -> State
exec (TAssert expr typ) state = 
  if cond
    then state
    else error $ "Assert failed for " ++ (show expr) 
      ++ " in state " ++ (show state)
  where
    B cond = eval expr state
exec (TAssign var expr) state = 
  if isVariable var state 
    then changeValue var val state 
    else addVariable var val state
  where val = eval expr state
exec wstmt@(TWhile expr stmt) state = state' where
  B cond = eval expr state
  state' = if cond then exec wstmt (exec stmt state) else state
exec (TIfStmt expr stmt1 stmt2) state = state' where
  B cond = eval expr state
  state' = if cond then exec stmt1 state else exec stmt2 state
exec seq@(TSequence (stmt:stmts)) state =
  exec (TSequence stmts) (exec stmt state)
exec seq@(TSequence []) state = state
-- exec stmt state = error $ "No such statement " ++ (show stmt)

-----------------------

-- | Show and evaluate expressions
showeval :: State -> TExpr -> String
showeval state e = 
  "eval (T" ++(show e) ++ ") (" ++ (show state) ++ ") = " ++ 
    show (eval e state) ++ "\n"

-- | Show and evaluate statements
showexec :: State -> TStmt -> String
showexec state stmt = do
  "exec (T" ++ (show stmt) ++ ") (" ++ (show state ) ++ ") = " 
  ++ (show state') ++ "\n"
  where
    state' = (exec stmt state)


-----------------------

-- | Validate euclidian division
validateEucliddiv :: (TStmt -> State -> State) -> XYQR -> Bool
validateEucliddiv exec xyqr = 
  getValue "q" state == I (fromIntegral(fst (snd xyqr))) && 
  getValue "r" state == I (fromIntegral(snd (snd xyqr))) 
  where
    statex  = addVariable "x" (I (fromIntegral(fst (fst xyqr)))) newState 
    statexy = addVariable "y" (I (fromIntegral(snd (fst xyqr)))) statex
    state = exec eucliddiv_tstmt statexy
unittestEuclidDiv = do
  print $ map (validateEucliddiv exec) eucliddiv_answers

-- | Validate computus
validateComputus :: (TStmt -> State -> State) -> Date -> Bool
validateComputus exec date = 
  getValue "month" state == I (fromIntegral(fst (snd date))) && 
  getValue "day" state == I (fromIntegral(snd (snd date))) 
  where
    state = exec computus_tstmt (addVariable "Y" (I (fromIntegral(fst date))) newState)

unittestComputus = do
  print $ map (validateComputus exec) computus_answers

-----------------------
-- | Computing Easter Sunday month and day for a given year
mainComputus year = do
  let state = exec computus_tstmt (addVariable "Y" (I (fromIntegral year)) newState)
  let I month = getValue "month" state
  let I day = getValue "day" state
  putStrLn $ "Easter Sunday is " ++ (show year) ++ "–" ++ (show month) ++ "–" ++ (show day)
