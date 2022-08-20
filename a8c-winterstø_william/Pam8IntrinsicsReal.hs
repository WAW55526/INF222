-- | A selection of documented real function declarations and their semantics.
-- Also instantiates the calculator template for the intrinsics.
--
-- Author Magne Haveraaen
-- Since 2020-03-21

module Pam8IntrinsicsReal where

-- Use signatures
import Pam8Signature

-- Use the calculator template for signatures and function models.
import Pam8CalculatorTemplate

-----------------------
-- | Declaration of operations and their argument list and return type.
realOperations :: Signature
realOperations
  = ([("Real","Haskell's Double approximation to real numbers")],
     [ 
      ("Add", ["Real","Real"],"Real","Add two reals"),
      ("Mult", ["Real","Real"],"Real","Multiply two reals"),
      ("Sub", ["Real","Real"],"Real","Subtract two reals"),
      ("Neg", ["Real"],"Real","Negate (change sign of) a real"),
      ("Slash", ["Real","Real"],"Real","Division of two reals"),
      ("Abs", ["Real"],"Real","The absolute value (positive) of a real"),
      ("Sqr", ["Real"],"Real","Squaring a real"),
      ("Pi", [],"Real","The pi constant"),
      ("Sin", ["Real"],"Real","Sine of a real (radians)"),
      ("Cos", ["Real"],"Real","Cosine of a real (radians)"),
      ("Exp", ["Real"],"Real","Exponent of a real (natural exponent)"),
      ("Ln", ["Real"],"Real","Natural logarithm of a real"),
      ("Sqrt", ["Real"],"Real","Square root of a non-negative real"),
      ("Arctan", ["Real"],"Real","Arc tangent (inverse tangent) of a real (radians)\nfirst and fourth quadrant"),
      ("Idiv", ["Real","Real"],"Real","Integer division rounded towards zero"),
      ("Rem", ["Real","Real"],"Real","Remainder when doing integer division"),
      ("Succ", ["Real"],"Real","Successor, adding 1 to a number"),
      ("Pred", ["Real"],"Real","Predessor, subtracting 1 from a number")
    ])

-----------------------
-- | Semantics of chosen real operations.
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


-- | Function creating test data with at most 11 elements.
realTestData :: [TypeName] -> [Double]
realTestData params = take (length params) [10..20]

-----------------------
-- | Unit test of the realOperations:
-- for each declaration in the signature check that there is a corresponding semantics.
unittestPam8IntrinsicsReal = do
  print $ "-- unittestPam8IntrinsicsReal --"
  -- print $ checkFunModel realOperations realSemantics realTestData
  print $
    -- Expected result of calling the declared functions on relevant argument lists.
    if (checkFunModel realOperations realSemantics realTestData) ==
        [21,110,-1,-10,10/11,10,100,pi,sin 10,cos 10,exp 10,log 10,sqrt 10,atan 10,0,10,11,9]
    then "Unit tests hold"
    else "Tests failed"


-----------------------
-- | Interactive calculator with variables an  d given selection of real operations.
main = do
  putStrLn $ "-- Scientific calculator for reals --"
  calculatorTemplate realOperations realSemantics realTestData
