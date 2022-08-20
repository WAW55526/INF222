-- | BIPL3Interpreter: big step operational semantics of BIPL1, 
-- using environment and store for the state.
-- A permissive semantics with implicit declarations.
-- @date 2022-02-10
-- @author Magne Haveraaen

module BIPL3Interpreter where

import BIPL3AST

import BIPL3State

-----------------------

-- | Evaluate expressions
eval :: Expr -> State -> Value
eval (IL i) state = I (fromIntegral i)
eval (Plus e1 e2) state = I (e1' + e2') where
  I e1' = eval e1 state
  I e2' = eval e2 state
eval (Mult e1 e2) state = I (e1' * e2') where
  I e1' = eval e1 state
  I e2' = eval e2 state
eval (Uminus e1) state = I (- e1') where
  I e1' = eval e1 state
eval (Div e1 e2) state = I (div e1' e2') where
  I e1' = eval e1 state
  I e2' = eval e2 state
eval (Mod e1 e2) state = I (mod e1' e2') where
  I e1' = eval e1 state
  I e2' = eval e2 state
eval (BL b) state= B b
eval (Or e1 e2) state = B (e1' || e2') where
  B e1' = eval e1 state
  B e2' = eval e2 state
eval (And e1 e2) state = B (e1' && e2') where
  B e1' = eval e1 state
  B e2' = eval e2 state
eval (Not e1) state = B (not e1') where
  B e1' = eval e1 state
eval (Choice e0 e1 e2) state = (if e0' then e1' else e2') where
  B e0' = eval e0 state
  e1' = eval e1 state
  e2' = eval e2 state
eval (Equal e1 e2) state = B (e1' == e2') where
  I e1' = eval e1 state
  I e2' = eval e2 state
eval (Le e1 e2) state = B (e1' <= e2') where
  I e1' = eval e1 state
  I e2' = eval e2 state
eval (VarExp var) state = getValue var state


-- | Execute statements
exec :: Stmt -> State -> State
exec (Assert expr) state = 
  if cond
    then state
    else error $ "Assert failed for " ++ (show expr) 
      ++ " in state " ++ (show state)
  where
    B cond = eval expr state
exec (Assign var expr) state = 
  if isVariable var state 
    then changeValue var val state 
    else addVariable var val state
  where val = eval expr state
exec wstmt@(While expr stmt) state = state' where
  B cond = eval expr state
  state' = if cond then exec wstmt (exec stmt state) else state
exec (IfStmt expr stmt1 stmt2) state = state' where
  B cond = eval expr state
  state' = if cond then exec stmt1 state else exec stmt2 state
exec seq@(Sequence (stmt:stmts)) state =
  exec (Sequence stmts) (exec stmt state)
exec seq@(Sequence []) state = state


-----------------------

-- | Show and evaluate expressions
showeval :: State -> Expr -> String
showeval state e = 
  "eval (" ++(show e) ++ ") (" ++ (show state) ++ ") = " ++ 
    show (eval e state) ++ "\n"

-- | Show and evaluate statements
showexec :: State -> Stmt -> String
showexec state stmt = do
  "exec (" ++ (show stmt) ++ ") (" ++ (show state ) ++ ") = " 
  ++ (show state') ++ "\n"
  where
    state' = (exec stmt state)


-----------------------

-- | Validate euclidian division
validateEucliddiv :: (Stmt -> State -> State) -> XYQR -> Bool
validateEucliddiv exec xyqr = 
  getValue "q" state == I (fromIntegral(fst (snd xyqr))) && 
  getValue "r" state == I (fromIntegral(snd (snd xyqr))) 
  where
    statex  = addVariable "x" (I (fromIntegral(fst (fst xyqr)))) newState 
    statexy = addVariable "y" (I (fromIntegral(snd (fst xyqr)))) statex
    state = exec eucliddiv_stmt statexy
unittestEuclidDiv = do
  print $ validateEucliddiv exec ((49,7),(7,0)) 
  print $ map (validateEucliddiv exec) eucliddiv_answers

-- | Validate computus
validateComputus :: (Stmt -> State -> State) -> Date -> Bool
validateComputus exec date = 
  getValue "month" state == I (fromIntegral(fst (snd date))) && 
  getValue "day" state == I (fromIntegral(snd (snd date))) 
  where
    state = exec computus_stmt (addVariable "Y" (I (fromIntegral(fst date))) newState)

unittestComputus = do
  print $ validateComputus exec (2022,(4,17))
  print $ map (validateComputus exec) computus_answers

-----------------------
-- | Computing Easter Sunday month and day for a given year
mainComputus year = do
  let state = exec computus_stmt (addVariable "Y" (I (fromIntegral year)) newState)
  let I month = getValue "month" state
  let I day = getValue "day" state
  putStrLn $ "Easter Sunday is " ++ (show year) ++ "–" ++ (show month) ++ "–" ++ (show day)
