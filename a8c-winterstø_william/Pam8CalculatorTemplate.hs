-- | Calculator template for variable based integer calculator with explicit signature documentation.
--
-- Author Magne Haveraaen
-- Since 2020-03-19

module Pam8CalculatorTemplate where

-- Use variable calculator with explicit signatures.
import Pam8Signature
import Pam8SignatureAST

-- Use interpreter with state and explicit signatures.
import Pam8SignatureInterpreter
import Pam8State

-- Use editable console.
import System.Console.Haskeline
-- Use a Haskell parser wrapped in Maybe.
import Text.Read (readMaybe)


-----------------------
-- | Interactive calculator parameterised by function models.
-- First it is checked that the function model accepts the signature.
-- Then for every user provided statement, the calculator checks for defined variables and functions.
calculatorTemplate :: (Show valuedomain, Read valuedomain) =>
  Signature -> FunModel valuedomain -> TestData valuedomain -> IO ()
calculatorTemplate intrinsics@(types,fundecls) funmod testfun = do
  putStrLn $ "Interactive calulator with variables and the following intrinsic functions"
  -- Check consistency of intrinsic signature
  if checkSignature intrinsics == []
    then putStrLn ""
    else error $ "Error: missing type declarations " ++ (show $ checkSignature intrinsics)
          ++ " for intrinsic signature " ++ (show intrinsics)
  -- Validate that all intrinsic functions have a model in funmod
  if length fundecls == length (checkFunModel intrinsics funmod testfun)
    then putStrLn $ signatureToString intrinsics
    else error $ "Not all intrinsic functions have a model, sig=" ++ (show intrinsics)
  runInputT defaultSettings (loop newState)
  where
  -- Parses and executes CalcStmtAST and prints what happens.
  -- The recursive call to loop must update the state.
  loop state = do
    input <- getInputLine "¢ "
    case input of
      Nothing -> return ()
      Just "" ->
        do outputStrLn $ "Finished" ; return ()
      Just "show" -> 
        do outputStrLn $ "state = " ++ (show state) ; loop state
      Just str -> do
        case readMaybe str of
          Nothing -> do
            outputStrLn $ "Not a statement: " ++ str
            loop state
          Just stmt@(SetVar vname expr)|isDeclared vname state -> do
            outputStrLn $ "Error: output variable " ++ (show vname) ++ " already exists."
            checkExpression state intrinsics expr
            loop state
          Just stmt@(SetVar vname expr)|[]<-allDeclared state expr,typeCheckExpr intrinsics expr==[] -> do
            outputStrLn $ "SetVar " ++ (show vname) ++ " = " ++ (show $ evaluate funmod state expr)
            loop $ execute funmod stmt state
          Just stmt@(SetVar vname expr) -> do
            let undeclared = allDeclared state expr
            checkExpression state intrinsics expr
            loop state
          Just stmt@(AssVar vname expr)|isDeclared vname state,[]<-allDeclared state expr,typeCheckExpr intrinsics expr==[] -> do
            outputStrLn $ "AssVar " ++ (show vname) ++ " = " ++ (show $ evaluate funmod state expr)
            loop $ execute funmod stmt state
          Just stmt@(AssVar vname expr) -> do
            if isDeclared vname state
              then outputStr ""
              else outputStrLn $ "Error: output variable " ++ (show vname) ++ " has not been declared."
            checkExpression state intrinsics expr
            loop state
  -- | Check if an expression uses only declared functions and declared variables.
  checkExpression :: State valuedomain -> Signature -> CalcExprAST valuedomain -> InputT IO ()
  checkExpression state intrinsics expr = do
    let undeclared = typeCheckExpr intrinsics expr
    if undeclared == []
      then outputStr ""
      else outputStrLn $ "Syntax error: expression is not compatible with signature, undeclared functions are " ++ (show undeclared)
    let undeclared = allDeclared state expr
    if undeclared == []
      then outputStr ""
      else outputStrLn $ "Error: expression contains undeclared variables " ++ (show undeclared)


-----------------------
-- | Pretty print a signature
signatureToString :: Signature -> String
signatureToString sig@(types,fundecls) =
  -- Print type declarations with documentation
  foldl (++) "" 
    (map 
     (\(typ,doc) -> (printdoc doc ++ "  type " ++ typ ++ "\n")) 
     types
    ) ++
  -- Print function declarations with documentation
  foldl (++) "" 
    (map 
     (\(fn,args,res,doc) -> (printdoc doc ++ "  " ++ fn ++ " :: " ++
                         (foldr listargcomma "" args) ++ " -> " ++ res ++ "\n")) 
     fundecls
    )

-- | Create a (multiline) documentation with "  -- |" prefix at each line.
printdoc :: DocString -> String
printdoc doc = res
   where
     strs = splitOn '\n' doc
     lines = map (\str -> "  -- | " ++ str ++ "\n") strs
     res = foldr (++) "" lines

-- | Split a list into a list of lists on the delimiter element.
-- For instance splitOn 0 [1,2,0,4,0,0,5,7] = [[1,2],[4],[],[5,7]]
splitOn :: (Foldable t, Eq a) => a -> t a -> [[a]]
splitOn delimiter list = foldr f [[]] list
  where
    f c (x:xs) | c == delimiter = []:(x:xs)
               | otherwise = (c:x):xs


-- | Insert a comma between two nonempty strings.
listargcomma :: String -> String -> String
listargcomma "" "" = ""
listargcomma str1 "" = str1
listargcomma "" str2 = str2
listargcomma str1 str2 = str1 ++ ", " ++ str2
