-- Task 8.2.2
-- Author William A. Winterstø
-- Since 27.04.2022

module IntrinsicsYarn0 where

-- Use signatures
import Pam8Signature

-- Use the calculator template for signatures and function models.
import Pam8UCalculatorTemplate

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

-----------------------
-- | Unit test of the yarnOperations:
-- for each declaration in the signature check that there is a corresponding semantics.
unittestIntrinsicsYarn0 = do
  print $ "-- unittestIntrinsicYarn0 --"
  -- print $ checkFunModel realOperations realSemantics realTestData
  print $
    -- Expected result of calling the declared functions on relevant argument lists.
    if checkFunModel yarnOperations realSemantics yarnTestData ==
        [2000, 0, 80, 500, 0.25, 800, 0, 4000, 1, 160, 2.5, 200, 0, 1000]
    then "Unit tests hold"
    else "Tests failed"


-----------------------
-- | Interactive calculator with variables and given selection of yarn operations.

-- ANALYSIS:
-- Running the examples in IntrinsicsYarn0 gives the same result as in Pam8UCalculatorTemplate.
-- This is what I expected since IntrinsicsYarn0 never typechecks the Typenames of the parameters so the calculation is done no matter what the type is. 
main = do
  putStrLn $ "-- Scientific calculator for yarns --"
  calculatorTemplate yarnOperations realSemantics yarnTestData
