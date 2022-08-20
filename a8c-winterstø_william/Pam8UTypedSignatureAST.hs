-- Task 8.2.3
-- Author William A. Winterstø
-- Since 29.04.2022

-- DISCUSSION (8.2.3.6):
-- Everything except the last point about "numerical values should be reasonable with respect to their unit/type" is taken care of by inferTypedExprAST.
-- The only thing that's done with the number ranges is the tests in IntrinsicsYarn0, they produce relevant numbers.
-- As for dangers assosiated with source code injection: If i were to fully develop the calculator i would have to add a function
-- that filters the input from the users. If the input does not match what is expected there would be an error.
-- As long as the imput follows the expected parameters there is no danger of malicious haskell code being executed.  


module Pam8UTypedSignatureAST where

-- Based on signatures and function models
import Pam8Signature
import Pam8USignatureAST

-----------------------
-- | Expressions for a calculator with variables.
-- The calculator supports literals Lit for any value domain,
-- an open ended set of primitive functions Fun, and
-- an open ended set of variables Var.
data TCalcExprAST valuedomain
    = TLit valuedomain UnitName TypeName
    | TFun FunName [TCalcExprAST valuedomain] TypeName
    | TVar VarName TypeName
    deriving (Eq, Read, Show)

-- | Statement for declaring (setting) and changing (assigning) a variable
data TCalcStmtAST valuedomain
    = TSetVar VarName (TCalcExprAST valuedomain)
    | TAssVar VarName (TCalcExprAST valuedomain)
    deriving (Eq, Read, Show)


-- | Infer a typed expression AST from an untyped AST, using
-- a signature, a unit environment and a variable environment.
-- If the type cannot be inferred, an empty string is used.
inferTypedExprAST :: Show valuedomain => Signature -> UnitEnvironment -> VarEnvironment -> CalcExprAST valuedomain -> TCalcExprAST valuedomain
inferTypedExprAST _ unitEnv _ (Lit val unitName) = TLit val unitName (checkEnv unitName unitEnv)
inferTypedExprAST sig unitEnv varEnv fcall@(Fun fn exprs) = TFun fn tExprs typ
    where
        tExprs = map (inferTypedExprAST sig unitEnv varEnv) exprs
        typ = typeOfFcall sig fn tExprs
inferTypedExprAST _ unitEnv varEnv (Var varName) = TVar varName (checkEnv varName varEnv)

typeOfFcall :: Show valuedomain => Signature -> FunName -> [TCalcExprAST valuedomain] -> TypeName
typeOfFcall (_,[]) _ _ = ""
typeOfFcall sig@(types,(fname,params,res,_):fundecls) fn tExprs = 
    if fname == fn && params == typesOfExprs tExprs
        then res
        else typeOfFcall (types,fundecls) fn tExprs

typesOfExprs :: Show valuedomain => [TCalcExprAST valuedomain] -> [TypeName]
typesOfExprs [] = []
typesOfExprs ((TLit _ _ typ):cExprs) = typ : typesOfExprs cExprs
typesOfExprs ((TFun _ _ typ):cExprs) = typ : typesOfExprs cExprs
typesOfExprs ((TVar _ typ):cExprs) = typ : typesOfExprs cExprs

checkEnv :: String -> TypeEnvironment -> TypeName
checkEnv name [] = ""
checkEnv name ((string,typeName):xs) = if name == string then typeName else checkEnv name xs

-- | Infer a typed statement AST from an untyped AST, using
-- a signature, a unit environment and a variable environment.
-- If the type cannot be inferred, an empty string is used.
inferTypedStmtAST :: Show valuedomain => Signature -> UnitEnvironment -> VarEnvironment -> CalcStmtAST valuedomain -> TCalcStmtAST valuedomain
inferTypedStmtAST sig unitEnv varEnv (SetVar varName calcExpr) = TSetVar varName (inferTypedExprAST sig unitEnv varEnv calcExpr)
inferTypedStmtAST sig unitEnv varEnv (AssVar varName calcExpr) = TAssVar varName (inferTypedExprAST sig unitEnv varEnv calcExpr)

-----------------------
-- | Check the safety of the typedAST.
-- Returns a list of functionsNames that resulted in getting no type.
funcCallsWithNoType :: TCalcExprAST valuedomain -> [FunName]
funcCallsWithNoType (TFun functionName exprs "") = functionName : funcCallsWithNoType' exprs
funcCallsWithNoType _ = []

funcCallsWithNoType' :: [TCalcExprAST valuedomain] -> [FunName]
funcCallsWithNoType' [] = []
funcCallsWithNoType' ((TFun functionName _ ""):exprs) = functionName : funcCallsWithNoType' exprs
funcCallsWithNoType' (_:exprs) = funcCallsWithNoType' exprs

-----------------------
-- | Unit test for inferTypedExprAST, inferTypedStmtAST and funcCallsWithNoType
-- Contains inputs as well as expected result.
unittestPam8UTypedSignatureAST = do
  print $ "-- unittestPam8UTypedSignatureAST --"

  let input1 = Fun "Slash" [(Fun "Add" [(Lit 5 "NOK"), (Lit 3 "NOK")]), (Var "len")]
  let result1 = TFun "Slash" [TFun "Add" [TLit 5 "NOK" "Cost",TLit 3 "NOK" "Cost"] "Cost",TVar "len" "Length"] "UnitCost"

  let input2 = SetVar "func1" (Fun "Slash" [(Fun "Add" [(Lit 5 "NOK"), (Lit 3 "NOK")]), (Var "len")])
  let result2 = TSetVar "func1" (TFun "Slash" [TFun "Add" [TLit 5 "NOK" "Cost",TLit 3 "NOK" "Cost"] "Cost",TVar "len" "Length"] "UnitCost")

  let input3 = TFun "Slash" [TFun "Add" [TLit 5 "NOK" "Cost",TLit 3 "NOK" ""] "",TLit 2 "meter" "Length"] ""
  let result3 = ["Slash", "Add"]

  let func1 = inferTypedExprAST yarnOperations yarnUnits [("price", "Cost"), ("len", "Length"), ("wallet", "NOK")] input1
  let func2 = inferTypedStmtAST yarnOperations yarnUnits [("price", "Cost"), ("len", "Length"), ("wallet", "NOK")] input2
  let func3 = funcCallsWithNoType input3

  let test1 = func1 == result1
  let test2 = func2 == result2
  let test3 = func3 == result3
  
  print $ if test1 && test2 && test3 then "PASSED" else "Not PASSED"







-- Putting this here was the easiest way to resolve a import loop


-- | Declaration of operations and their argument list and return type.
yarnOperations :: Signature
yarnOperations =
    ([
      ("Amount",   "The number of rolls of yarn (amount)"),
      ("Cost",     "The total cost for a purchase of yarn (NOK)"),
      ("Density",  "Density of thread (gram/meter)"),
      ("Length",   "Length of a roll of yarn (meter)"),
      ("UnitCost", "The unit cost for yarn (NOK/meter)"),
      ("Weight",   "Weight of a roll of yarn (gram)")
     ],
     [
      ("Add",   ["Cost","Cost"],       "Cost",     "Add two costs"),
      ("Sub",   ["Cost","Cost"],       "Cost",     "Subtract two costs"),
      ("Mult",  ["Density","Length"],  "Weight",   "Compute weight from density and length"),
      ("Slash", ["Weight","Density"],  "Length",   "Compute length from weight and density"),
      ("Slash", ["Weight","Length"],   "Density",  "Compute density"),
      ("Add",   ["Length","Length"],   "Length",   "Add two lengths"),
      ("Sub",   ["Length","Length"],   "Length",   "Subtract two lengths"),
      ("Mult",  ["Length","Amount"],   "Length",   "Multiply length by amount"),
      ("Slash", ["Length","Length"],   "Amount",   "Compute amount"),
      ("Mult",  ["UnitCost","Length"], "Cost",     "Compute cost based on length"),
      ("Slash", ["Cost","Length"],     "UnitCost", "Compute unit cost from cost and length of yarn"),
      ("Add",   ["Weight","Weight"],   "Weight",   "Add two weights"),
      ("Sub",   ["Weight","Weight"],   "Weight",   "Subtract two weights"),
      ("Mult",  ["Weight","Amount"],   "Weight",   "Multiply weight by amount")      
    ])

-- | The units for the yarn calculator.
-- Note that some types have multiple units, e.g., "gram" and "g".
yarnUnits :: UnitEnvironment
yarnUnits = [("amount","Amount"),
             ("NOK","Cost"),
             ("gram/meter","Density"), ("g/m","Density"),
             ("meter","Length"), ("m","Length"),
             ("NOK/meter","UnitCost"),
             ("gram","Weight"), ("g","Weight")]

-----------------------
-- | Semantics of chosen yarn operations.
realSemantics :: FunModel Double
realSemantics "Add" [i1,i2] = i1 + i2
realSemantics "Mult" [i1,i2] = i1 * i2
realSemantics "Sub" [i1,i2] = i1 - i2
realSemantics "Neg" [i] = - i
realSemantics "Slash" [i1,0]
  = error $ "Cannot do real division (slash) of " ++ (show i1) ++ " by 0."
realSemantics "Slash" [i1,i2] = i1 / i2
realSemantics "Abs" [i] = abs i
realSemantics "Sqr" [i] = i * i
realSemantics "Pi" [] = pi
realSemantics "Sin" [i] = sin i
realSemantics "Cos" [i] = cos i
realSemantics "Exp" [i] = exp i
realSemantics "Ln" [i] = log i
realSemantics "Sqrt" [i] = sqrt i
realSemantics "Arctan" [i] = atan i
realSemantics "Idiv" [i1,0]
  = error $ "Cannot do integer division of " ++ (show i1) ++ " by 0."
realSemantics "Idiv" [i1,i2] = fromIntegral (truncate (i1 / i2))
realSemantics "Rem" [i1,0] 
  = error $ "Cannot do remainder of " ++ (show i1) ++ " by 0."
realSemantics "Rem" [i1,i2] = i1 - fromInteger (truncate (i1 / i2)) * i2
realSemantics "Succ" [i] = i + 1
realSemantics "Pred" [i] = i - 1
realSemantics fname alist 
  = error $ "Unknown function name/arg list " ++ (show fname) ++ " " ++ (show alist)


-- | Function creating test data.
-- An attempt is made to adapt to the expected ranges for each of the yarn types.
yarnTestData :: [TypeName] -> [Double]
yarnTestData [] = []
yarnTestData ("Amount":ps) = 10 : yarnTestData ps
yarnTestData ("Cost":ps) = 1000 : yarnTestData ps
yarnTestData ("Density":ps) = 0.2 : yarnTestData ps
yarnTestData ("Density":ps) = 0.2 : yarnTestData ps
yarnTestData ("Length":ps) = 400 : yarnTestData ps
yarnTestData ("Length":ps) = 400 : yarnTestData ps
yarnTestData ("UnitCost":ps) = 0.4 : yarnTestData ps
yarnTestData ("Weight":ps) = 100 : yarnTestData ps
yarnTestData ("Weight":ps) = 100 : yarnTestData ps
yarnTestData (_:ps) = 1 : yarnTestData ps -- will never happen, done to remove warning for not catching pattern