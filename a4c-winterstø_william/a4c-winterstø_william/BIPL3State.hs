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

module BIPL3State where

-- Use Haskell's array data structure
import Data.Array
-- Use Haskell's 8bit word for locations
import Data.Word (Word8)
-- Use Haskell's 16bit integers for integer
import Data.Int (Int16)

-----------------------
-- | Value domain
data Value = U
  | I VInteger
  | B Bool 
  | L Location 
  deriving (Show,Eq,Read)

type VInteger = Int16
type Location = Word8

-----------------------
-- | A state is an environment of variable-store index associations, and
-- a store which at each store index keeps a value (for that variable).
type State = (Environment,Store)

-- | A new state is an empty environment with an empty store.
newState :: State
newState = (emptyEnvironment,emptyStore)

-- | Checks if a name is registered as a variable in the environment
isVariable :: String -> State -> Bool 
isVariable vname (env,store) = lookup vname env /= Nothing

-- | Gets the value linked to the variable in the state.
getValue :: String -> State -> Value
getValue vname (env,store) = 
    getStoreValue (getVariableLocation vname env) store

-- | Add a new variable with value to the state.
addVariable :: String -> Value -> State -> State
addVariable vname value (env,store) = (env',store')
  where
    store' = enlargeStore value store
    env' = addVariableToEnvironment vname (availableLocation store) env

-- | Creates a new variable name as an alias for an existing variable location.
createAlias :: String -> String -> State -> State
createAlias aliasvar vname (env,store) = (env',store)
  where
    env' = addVariableToEnvironment aliasvar (getVariableLocation vname env) env

-- | Changes the value associated with a known variable.
changeValue :: String -> Value -> State -> State
changeValue vname value (env,store) = (env,setStoreValue (getVariableLocation vname env) value store)

-- | Get current free location and environment
getFreeLocation :: State -> (Location,Environment)
getFreeLocation (env,store) = (availableLocation store,env)

-- | Reset free location and related environment
resetFreeLocation :: (Location,Environment) -> State -> State
resetFreeLocation (oldloc,oldenv) (env,store) = 
  (oldenv, setFreeStoreLocation oldloc store)


-----------------------
-- | An Environment for variables.
-- It stores an association list of distinct variable names and their store index.
-- As such, it can be searched by the Haskell standard function 
--   lookup :: Eq a => a -> [(a, b)] -> Maybe b
type Environment = [(String,Location)]

-- | Defines an empty environment
emptyEnvironment :: Environment
emptyEnvironment = []

-- | Add a new variable (and a store index) to the environment.
addVariableToEnvironment :: String -> Location -> Environment -> Environment
addVariableToEnvironment vname ind env =
  case lookup vname env of
    Just loc -> error $ "New variable " ++ (show (vname,ind)) 
            ++ " already registered in " ++ (show env)
    Nothing ->  (vname,ind):env 

-- | Gets the location of a stored variable.
getVariableLocation :: String -> Environment -> Location
getVariableLocation vname env =
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
emptyStore = (maxBound,array (minBound,maxBound) [(i,U) | i <- [minBound..maxBound]])

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
enlargeStore :: Value -> Store -> Store
enlargeStore value store@(freeloc,arr) = (freeloc',arr//[(freeloc,value)])
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

-----------------------
-- | Unit tests for State and values.
unittestState = do
  print $ "-- unittestState"
  -- putStrLn \$ "Empty state = " ++ (show newstate)
  let state1 = addVariable "v1" (I 1) newState
  let state2 = addVariable "v2" (L 4) state1
  let state3 = addVariable "v3" (I 9) state2
  let locenv3 = getFreeLocation state3
  let state4 = changeValue "v2" (I 25) state3
  let state5 = addVariable "v4" (I 19) state4
  let state6 = addVariable "v5" (I 93) state5
  let state7 = createAlias "v6" "v1" state6
  let state8 = changeValue "v6" (I 42) state7
  let state3' = resetFreeLocation locenv3 state8
  -- putStrLn $ "State8 = " ++ (show state8)
  -- putStrLn $ "State3' = " ++ (show state3')
  -- print $ "Value of: I (-32768 - 32726) = " ++ show (I (-32768 - 32726))
  -- print $ "Value of: I (32767 + 32727) = " ++ show (I (32767 + 32727))
  putStrLn $
    if I 42 == getValue "v1" state8
    && I 25 == getValue "v2" state8
    && I  9 == getValue "v3" state8
    && I 19 == getValue "v4" state8
    && I 93 == getValue "v5" state8
    && I 42 == getValue "v6" state8
    && I 42 == I (-32768 - 32726)
    && I (-42) == I (32767 + 32727)
    then "Unit tests hold"
    else "Tests failed"
