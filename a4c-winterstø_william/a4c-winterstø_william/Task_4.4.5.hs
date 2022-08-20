import BIPL3AST
import BIPL3Interpreter
import BIPL3State
import BIPL4TypedAST
import Data.Int (Int16)

-- | Procedure declaration
data ProcedureDeclaration = Proc
    String -- Name of the procedure
    [Parameter] -- Parameter list
    [VarDecl] -- Local variables
    Stmt -- Statement part
    deriving (Show, Eq, Read)
-- | Procedure parameters: mode and variable declaration
type Parameter = (Mode, VarDecl)
-- | Parameter modes: observe, update, output
data Mode = Obs | Upd | Out
    deriving (Show, Eq, Read)
-- | Variable declaration: variable name and its type
type VarDecl = (Var,Type)

-- | Perform a procedure, after the parameters have been set up:
-- • augment the state with local declarations
-- • execute the statement and update the state
-- • reset the state to remove the local declarations
perform :: ProcedureDeclaration -> State -> State
perform (Proc name params varDs stmt) state = (oldEnv,newStore)
    where
        oldEnv = fst state
        store = snd state
        newEnv = fst (addCallStateLocals varDs state)
        stateToExec = (newEnv, store)
        newStore = snd (exec stmt stateToExec)

{-
perform :: ProcedureDeclaration -> State -> State
perform (Proc name par varDecl stmt) oldState@(oldEnv, oldStore) = (oldEnv, nStore)
    where
        state = addCallStateLocals varDecl oldState
        (newEnv, nStore) = exec stmt state
-}

-- | Add local variables to the call state
addCallStateLocals :: [VarDecl] -> State -> State
addCallStateLocals [] state = state
addCallStateLocals (varD:varDs) state = addCallStateLocals varDs ((addVariableToEnvironment vname loc env), store)
    where
        env = fst state
        store = snd state
        vname = fst varD
        loc = availableLocation store

-- | Running a procedure:
-- • Initialise a new state with the input parameters initialised from the value list
-- • Perform the procedure in this state
-- • Extract the output values from the return state
run :: ProcedureDeclaration -> [Value] -> [Value]
run (Proc str params varDs stmt) vals = values
    where
        state = addCallStateInputs params vals newState
        updatedState = perform (Proc str params varDs stmt) state
        values = getReturnStateOutputs params updatedState
        

-- | Add the parameter variables to the state, and
-- initialise each input parameter with the corresponding value.
addCallStateInputs :: [Parameter] -> [Value] -> State -> State
addCallStateInputs [] _ state = state
addCallStateInputs _ [] state = state
addCallStateInputs ((Out, _): params) (value:values) state = addCallStateInputs params values state
addCallStateInputs (param:params) (value:values) state = addCallStateInputs params values updatedState
    where
        vname = fst (snd param)
        updatedState = if isVariable vname state
                        then changeValue vname value state
                        else addVariable vname value state

-- | Get each output parameter’s value from the state
getReturnStateOutputs :: [Parameter] -> State -> [Value]
getReturnStateOutputs [] state = []
getReturnStateOutputs ((Obs, _): params) state = getReturnStateOutputs params state
getReturnStateOutputs (param:params) state = [value] ++ getReturnStateOutputs params state
    where
        env = fst state
        store = snd state
        vname = fst (snd param)
        loc = getVariableLocation vname env
        value = getStoreValue loc store

eucliddiv_proc :: ProcedureDeclaration
eucliddiv_proc = Proc "TestProc" [(Obs, ("x", IntegerType)), (Obs, ("y", IntegerType)), (Upd, ("r", IntegerType)), (Upd, ("q", IntegerType))] [] eucliddiv_stmt

mainEuclid :: Int16 -> Int16 -> IO()
mainEuclid x y = putStrLn ("Result: " ++ show result ++ "\nRemainder: " ++ show remainder)
    where
        ls = run eucliddiv_proc [I x, I y, I x, I 0]
        (I result) = head (tail ls)
        (I remainder) = head ls


        
