-- | PIPLInterpreter: big step operational semantics of PIPL
-- using environment and store for the state.

module PIPLInterpreter where

import BIPL3AST
import PIPLMeta
import qualified BIPL3State as S
import BIPL3State (Value(..), State)
import BIPL3MetaTypes

type FunModel = FunName -> [Value] -> Value

type ExecutionModel = (FunModel, Program)

getFunModel :: ExecutionModel -> FunModel
getFunModel = fst

getProgram :: ExecutionModel -> Program
getProgram = snd

-- ///////// EVALUATE EXPRESSIONS ////////////////

eval :: Expr -> FunModel -> State -> Value
eval (IL i) _ state = I (fromIntegral i)
eval (BL b) _ state= B b
eval (VarExp var) _ state = S.getValue var state
eval (CallFun op args) funModel state =
    funModel op $ map (\expr -> eval expr funModel state) args

-- ///////// EXECUTE STATEMENTS ////////////////

exec :: Stmt -> ExecutionModel -> State -> State
exec (Assert expr) (funModel, _) state =
  if cond
    then state
    else error $ "Assert failed for " ++ (show expr)
      ++ " in state " ++ (show state)
  where
    B cond = eval expr funModel state
exec (Assign var expr) (funModel, _) state = S.changeValue var val state
  where val = eval expr funModel state
exec wstmt@(While expr stmt) execModel@(funModel, _) state = state' where
  B cond = eval expr funModel state
  state' = if cond then exec wstmt execModel (exec stmt execModel state) else state
exec (IfStmt expr stmt1 stmt2) execModel@(funModel, _) state = state' where
  B cond = eval expr funModel state
  state' = if cond then exec stmt1 execModel state else exec stmt2 execModel state
exec seq@(Sequence (stmt:stmts)) execModel state =
  exec (Sequence stmts) execModel (exec stmt execModel state)
exec seq@(Sequence []) execModel state = state

exec (CallProc pName args) execModel@(funModel, program) state = finalState
  where
    (Proc _ params _ _) = getProc pName program
    stack = S.getStackFrame state
    evalArgs = map (\expr-> eval expr funModel state) args
    inputState = addCallStateInputs params evalArgs (S.clearEnvironment state)
    performState = perform (getProc pName program) execModel inputState
    runVals = getReturnStateOutputs params performState
    setStackState = S.setStackFrame stack performState
    finalState = findOutputValues params runVals args setStackState

{-
exec (CallProc pName args) execModel@(funModel, program) scrState = setStackState
  where
    (Proc _ params _ _) = getProc pName program
    stack = S.getStackFrame scrState
    dstState = S.clearEnvironment scrState
    inputState = addArgs args params funModel scrState dstState
    performState = perform (getProc pName program) execModel inputState
    setStackState = S.setStackFrame stack performState
-}

addArgs :: [Expr] -> [Parameter] -> FunModel -> State -> State -> State
addArgs [] [] _ _ dstState = dstState
addArgs (e:es) (p@(mode,(pVar, t)):ps) funModel scrState dstState =
  if mode == Obs
    then addArgs es ps funModel scrState (addCallStateInputs [p] [eval e funModel scrState] dstState)
    else addArgs es ps funModel scrState (S.copyReference (getVar e) pVar scrState dstState)

getVar :: Expr -> Var
getVar (VarExp v) = v
getVar _ = error "Can't copy reference to something other then VarExp"

findOutputValues :: [Parameter] -> [Value] -> [Expr] -> State -> State
findOutputValues [] [] [] state = state
findOutputValues (p:ps) values@(v:vs) (e:es) state = if paramMode p == Obs
                                                      then findOutputValues ps values es state
                                                      else findOutputValues ps vs es (findOutputValues' e v state)

findOutputValues' ::  Expr -> Value -> State -> State
findOutputValues' (VarExp var) value state = S.changeValue var value state
findOutputValues' _ value state = state

-- ///////// RUN PROGRAMS ////////////////

-- | Run a given procedure in the given program, while passing in the given
-- | values as arguments. Returns the values of the output parameters (upd/out)
-- | after the procedure has been run.
runProgram :: ExecutionModel -> ProcName -> [Value] -> [Value]
runProgram execModel@(funModel, prog) pName args =
  let proc@(Proc _ params _ _) = getProc pName prog
   in run proc execModel args

run :: ProcedureDeclaration -> ExecutionModel -> [Value] -> [Value]
run proc@(Proc _ params locals body) execModel args =
    let state = addCallStateInputs params args S.newState
        state' = perform proc execModel state
     in getReturnStateOutputs params state'

perform :: ProcedureDeclaration -> ExecutionModel -> State -> State
perform (Proc _ params locals body) prog state =
    let oldStackFrame = S.getStackFrame state
        stateWithLocals = addUninitVars (map varName locals) state
        stateAfterExec = exec body prog stateWithLocals
        stateAfterCleanup = S.setStackFrame oldStackFrame stateAfterExec
     in stateAfterCleanup

-- ///////// UTILITY FUNCTIONS ////////////////

-- | Add a list of variable names to the state, without initialising them.
addUninitVars :: [Var] -> State -> State
addUninitVars varNames initState = foldl addVar initState varNames
    where addVar state varName = S.allocateVariable varName state

addCallStateInputs :: [Parameter] -> [Value] -> State -> State
addCallStateInputs params args =
    let inputVars = getParamNames $ filter isInputParameter params
        outModeVars = getParamNames $ filter (not . isInputParameter) params
        isInputParameter (mode, _) = mode /= Out
        getParamNames = map (varName . paramVar)
        addVar state (varName, value) = S.addVariable varName value state
        addVars varNames values initState = foldl addVar initState (zip varNames values)
     in addUninitVars outModeVars . addVars inputVars args

getReturnStateOutputs :: [Parameter] -> State -> [Value]
getReturnStateOutputs params state =
    let outputVars = getParamNames $ filter isOutputParameter params
        isOutputParameter (mode, _) = mode /= Obs
        getParamNames = map (varName . paramVar)
     in map (\varName -> S.getValue varName state) outputVars
