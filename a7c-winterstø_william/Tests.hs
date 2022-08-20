{-# LANGUAGE FlexibleInstances, ScopedTypeVariables #-}
module Tests where

import Data.Either
import Control.DeepSeq
import Control.Monad (forM_)
import Control.Exception
import Data.Bool (bool)

import CheckAliasing (checkAliasing)
import CheckProcCall (checkProcCall)
import CheckExprType (checkType)

import ExampleProcedures
import ExampleExprs

import PIPLInterpreter
import BIPL3State
import BIPL3Functions
import BIPL3MetaTypes

-- ///////////////// TEST OF PROCEDURE INTERPRETER //////////////////////

testInterpreter :: IO ()
testInterpreter = do
    testFib

testFib = do
    let testCases = [ (1, 1)
                    , (2, 2)
                    , (3, 3)
                    , (4, 5)
                    , (5, 8) -- input 5 should yield output 8
                    ]

        runTestCase (input, expectedOutput) = do
            let [result] = runProgram (biplFunModel, fibonacciProgram) "fib" [ I input ]
            putStrLn $ "fib(" ++ show input ++ ") returned " ++ show result
                                ++ " (expected " ++ show expectedOutput ++ ")"

    -- Run all test cases.
    forM_ testCases runTestCase

-- ///////////////// TEST OF STATIC ANALYSERS //////////////////////

-- Here we run `checkAliasing p` for each procedure `p` among the test cases.
--
-- sillyAddProgram should fail because it has an aliasing problem.
-- fibonacciProgram should not fail.
testCheckAliasing :: IO ()
testCheckAliasing =
    let runTestCase (name, expected, subject) = do
            putStr $ "Aliasing check of " ++ name ++ ": "
            result <- catchError (checkAliasing subject)
            putStrLn $ resultMessage result False expected

        testCases = [ ("silly_add", False, sillyAddProgram)
                    , ("fibonacci", True, fibonacciProgram)
                    , ("aliasTest1Program", False, aliasTest1Program)
                    ]

     in forM_ testCases runTestCase

-- Here we run `checkType e (varTypes, biplFunSigs)` for each expression
-- `e` among the test cases, where varTypes is as defined below.
-- (I.e. check them in a context where variables x and y are defined, and the
-- defined functions are the ones in biplFunSigs).
--
-- e1, e2 and e4 should not fail the check as they are well formed (with regard
-- to biplFunSigs and the given varTypes).
-- e3 should fail because it tries to call `or` with an integer as the
-- second argument.
-- e5 should fail because it tries to add an integer to a boolean.
testCheckType :: IO ()
testCheckType =
    -- These will be our defined variables in the tests.
    -- x : integer, y : boolean.
    let varTypes = [("x", IntegerType), ("y", BoolType)]
        typeEnv = (varTypes, biplFunSigs)

        runTestCase (name, expected, subject) = do
            putStr $ "Type checking check of expression " ++ name ++ ": "
            result <- mapRight Just <$> catchError (checkType subject typeEnv)
            putStrLn $ resultMessage result Nothing expected

        testCases = [ ("e1", Just IntegerType, e1)
                    , ("e2", Just IntegerType, e2)
                    , ("e3", Nothing, e3)
                    , ("e4", Just IntegerType, e4)
                    , ("e5", Nothing, e5)
                    ]
     in forM_ testCases runTestCase

-- Here we run `checkProcCall p` for each procedure `p` among the test cases.
--
-- sillyAddProgram and fibonacciProgram should pass, because all procedure calls
-- in these programs are well formed.
-- badCall should fail, because the procedurecall in it is not well-formed
-- (badCall has an integer parameter, but it tries to call itself with a boolean).
testCheckProcCall :: IO ()
testCheckProcCall =
    let runTestCase (name, expected, subject) = do
            let execModel = (biplFunSigs, subject)
            putStr $ "Procedure call check of " ++ name ++ ": "
            result <- catchError (checkProcCall execModel)
            putStrLn $ resultMessage result False expected

        testCases = [ ("silly_add", True, sillyAddProgram)
                    , ("fibonacci", True, fibonacciProgram)
                    , ("badCall", False, badCallProgram)
                    ]
    in forM_ testCases runTestCase

-- ///////////////// UTILITY FUNCTIONS //////////////////////

-- | Print a result message after running a test.
-- result: The result value returned by the test.
-- errorValue: If the test threw an exception, what value should it be
--             interpreted as.
-- expected: The value we expected the test to return (if the analysis is
--           correctly implemented).
resultMessage :: (Eq a, ShowResult a) => Either String a -> a -> a -> String
resultMessage result errorValue expected =
    let resultValue = maybe errorValue id $ eitherToMaybe result
        eitherToMaybe (Left _) = Nothing
        eitherToMaybe (Right x) = Just x
        resultIsExpected = resultValue == expected
        errorMessage = case result of
                    Left err -> " (Returned error message: \"" ++ err ++ "\")"
                    Right _ -> ""
        expectation = if resultIsExpected
                         then ", as expected."
                         else "! But should have returned " ++ showResult expected ++ "!"
    in showResult resultValue ++ expectation ++ errorMessage

-- Catch errors thrown by the static analysis function, and interpret them as
-- if the test failed.
catchError :: forall a. a -> IO (Either String a)
catchError val = do
 val' <- try (evaluate val) :: IO (Either SomeException a)
 pure $ case val' of
   Left e -> Left $ head $ lines $ displayException e
   Right v -> Right v

mapRight _ (Left x) = Left x
mapRight f (Right x) = Right (f x)

class ShowResult a where
    showResult :: a -> String

instance Show a => ShowResult (Maybe a) where
    showResult Nothing = "error"
    showResult (Just x) = show x

instance ShowResult Bool where
    showResult False = "Fail"
    showResult True = "Pass"
