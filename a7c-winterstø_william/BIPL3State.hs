-- | State: for keeping track of the state of an imperative computation.
-- The state consists of an environment (declared variables) and a store (computer memory).
-- • The enviromnment is an association list of variable names and their store locations.
-- • The store is an array of values indeced by locations. It also has a free location pointer
--   keeping track of the unused part of the memory.
-- The state API is aimed at explicit allocation of variables.
-- It has the machinery for static scoping of variables, e.g.,
-- allowing nested blocks and procedure calls.
--
-- The value type has been hard coded for 16bit integers (65536 values)
-- with range -32768..32767 and wrap around semantics.
-- It also admits Haskell Bool, location indices and an undecided value.
--
-- Locations have been hard coded to 8bit unsigned integers (256 values)
-- with range 0..255. This is small enough to allow full store dumps.
--
-- Author Magne Haveraaen
-- Since 2020-03-19
-- Modified from Pamphlets01/CalculatorState

module BIPL3State
  ( Value(..)
  , State
  , newState
  , getValue
  , isVariable
  , addVariable
  , allocateVariable -- Add without assigning value
  , changeValue
  , getStackFrame -- Previously called getFreeLocation
  , setStackFrame -- Previosly called resetFreeLocation
  , copyReference
  , clearEnvironment
  )
  where

-- Use Haskell's array data structure
import Data.Array
-- Use Haskell's 8bit word for locations
import Data.Word (Word8)
-- Use Haskell's 16bit integers for integer
import Data.Int (Int16)

import BIPL3MetaTypes

-----------------------
-- | Value domain
data Value =
  U
  | I VInteger
  | B Bool
  | L Location
  deriving (Show,Eq,Read)

type VInteger = Int16
type Location = Word8

-----------------------
-- | A state is an environment of variable-store index associations, and
-- a store which at each store index keeps a value (for that variable).
newtype State = State (Environment, Store)
  deriving Show

-- | A new state is an empty environment with an empty store.
newState :: State
newState = State (emptyEnvironment,emptyStore)

-- | Checks if a name is registered as a variable in the environment
isVariable :: Var -> State -> Bool
isVariable vname (State ((Environment env), store)) = lookup vname env /= Nothing

-- | Gets the value linked to the variable in the state.
getValue :: Var -> State -> Value
getValue vname (State (env, store)) =
    getStoreValue (getVariableLocation vname env) store

-- | Add a new variable with value to the state.
addVariable :: Var -> Value -> State -> State
addVariable vname value (State (env, store)) = State (env', store'')
  where
    (varLoc, store') = enlargeStore store
    store'' = setStoreValue varLoc value store'
    env' = addVariableToEnvironment vname varLoc env

-- | Add a new variable (without a value) to the state.
allocateVariable :: Var -> State -> State
allocateVariable vname (State (env, store)) = State (env',store')
  where
    (varLoc, store') = enlargeStore store
    env' = addVariableToEnvironment vname varLoc env

-- | Creates a new variable name as an alias for an existing variable location.
createAlias :: Var -> Var -> State -> State
createAlias aliasvar vname (State (env, store)) = State (env',store)
  where
    env' = addVariableToEnvironment aliasvar (getVariableLocation vname env) env

-- | Copies the location of variable srcVar from one state into another, giving
-- | the new variable name dstVar.
copyReference :: Var -> Var -> State -> State -> State
copyReference srcVar dstVar (State (srcEnv, _)) (State (dstEnv, dstStore)) = State (addVariableToEnvironment dstVar (getVariableLocation srcVar srcEnv) dstEnv, dstStore)

-- | Changes the value associated with a known variable.
changeValue :: Var -> Value -> State -> State
changeValue vname value (State (env, store)) = State (env, setStoreValue (getVariableLocation vname env) value store)

newtype StackFrame = StackFrame (Location, Environment)

-- Get current stack frame (free location and environment)
getStackFrame :: State -> StackFrame
getStackFrame (State (env, store)) = StackFrame (availableLocation store, env)

-- Set stack frame (free location and environment)
setStackFrame :: StackFrame -> State -> State
setStackFrame (StackFrame (oldLoc, oldEnv)) (State (env, store)) =
  State (oldEnv, setFreeStoreLocation oldLoc store)

-- | Clear the environment of a state
clearEnvironment :: State -> State
clearEnvironment (State (_, store)) = State (emptyEnvironment, store)

-----------------------
-- | An Environment for variables.
-- It stores an association list of distinct variable names and their store index.
-- As such, it can be searched by the Haskell standard function
--   lookup :: Eq a => a -> [(a, b)] -> Maybe b
newtype Environment = Environment [(Var,Location)]
  deriving Show

-- | Defines an empty environment
emptyEnvironment :: Environment
emptyEnvironment = Environment []

-- | Add a new variable (and a store index) to the environment.
addVariableToEnvironment :: Var -> Location -> Environment -> Environment
addVariableToEnvironment vname ind (Environment env) =
  case lookup vname env of
    Just loc -> error $ "New variable " ++ (show (vname,ind))
            ++ " already registered in " ++ (show env)
    Nothing -> Environment $ (vname,ind):env

-- | Gets the location of a stored variable.
getVariableLocation :: Var -> Environment -> Location
getVariableLocation vname (Environment env) =
  case lookup vname env of
    Just loc -> loc
    Nothing ->  error $ "Variable " ++ vname ++ " not found in environment " ++ (show env)

-----------------------
-- | A Store is mainly an array of locations, defined by the pair:
-- • a location index for the next available location in the store
-- • an array indexed by location of values.
-- All location values are initialised to U (unknown).
-- Store locations are occupied from the maximum downwards to the minimum array index.
type Store = (Location,Array Location Value)

-- | Creates an empty store, its index range is defined by the Location type
emptyStore :: Store
emptyStore = (maxBound,array (minBound,maxBound) [(i, U) | i <- [minBound..maxBound]])

-- | Get the value stored for the given index.
getStoreValue :: Location -> Store -> Value
getStoreValue ind store@(freeloc,arr) =
  if low <= ind && ind <= high
    then arr ! ind
    else error $ "Not a store index " ++ (show ind) ++
      ", store bounds are " ++ (show low) ++ "<=" ++ (show high)
  where (low,high) = bounds arr

-- | Set a new value at the provided location.
setStoreValue :: Location -> Value -> Store -> Store
setStoreValue ind val store@(freeloc,arr) =
  if low <= ind && ind <= high
  then (freeloc,arr // [(ind,val)])
  else error $ "Not a store index " ++ (show ind) ++ " for " ++ (show val) ++
    ", store bounds are " ++ (show low) ++ "<=" ++ (show high)
  where (low,high) = bounds arr

-- | Allocate value at next available location and move free location pointer.
-- Includes a safety check so we do not wrap around when allocating new memory.
enlargeStore :: Store -> (Location, Store)
enlargeStore store@(freeloc,arr) = (freeloc, (freeloc', arr))
  where
    freeloc' = if minBound < freeloc
      then freeloc-1
      else error $ "Next location " ++ (show freeloc) ++ " out of bounds " ++ (show (bounds arr))

-- | Location in store available for allocation
availableLocation :: Store -> Location
availableLocation store = fst store

-- | Set free location in store, must be higher than current free location
setFreeStoreLocation :: Location -> Store -> Store
setFreeStoreLocation loc store@(freeloc,arr) = store' where
  store' = if freeloc <= loc
    then (loc,arr)
    else error $ "Setting free location into unused area, loc="
      ++ show loc ++ ", freeloc=" ++ show freeloc
