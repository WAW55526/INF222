-- | The notion of a signature and function model with documentation strings.
-- This provides a setting for an open ended set of intrinsic (built in) functions.
--
-- Author Magne Haveraaen
-- Since 2020-03-19

module Pam8Signature where

-----------------------
-- | A signature is a list of type and function declarations.
-- Each type used in a function declaration must be a declared type.
type Signature = ([TypeDeclaration],[FunDeclaration])

-- | A function declaration provides a function name, a list of parameter types, a return type
-- and a documentation string. The documentation string can have embedded newlines.
type FunDeclaration = (FunName,[TypeName],TypeName, DocString)

-- | A type declaration provides a type name and a documentation string. 
-- The documentation string can have embedded newlines.
type TypeDeclaration = (TypeName,DocString)

-- | The model for a function call is a mapping 
-- from the function name (String) and related argument list of valuedomain 
-- to a resulting valuedomain.
type FunModel valuedomain = FunName -> [valuedomain] -> valuedomain

-- | Creates a list of test data to match the parameter list of a function declaration.
type TestData valuedomain = [TypeName] -> [valuedomain]

-- | Type environment: an association list between a variable/unit/etc and its type.
-- Typically a variable may be declared multiple times with possibly different types,
-- while a unit name should only be declared once and thus have a unique type.
type TypeEnvironment = [(String,TypeName)]

-- | An association list between unit names and their types.
-- Each unit name should only have one type.
type UnitEnvironment = TypeEnvironment

-- | An association list between variable names and their types.
-- A variable may be redeclared.
type VarEnvironment = TypeEnvironment

-----------------------
-- | Differentiating between the different purposes for strings

-- | Function names
type FunName = String
-- | Variable names
type VarName = String
-- | Type names
type TypeName = String
-- | Unit names
type UnitName = String
-- | Documentation strings
type DocString = String

-----------------------
-- | Consistency check for signatures:
-- Each type name in a function declaration must be a declared type.
-- Returns a list of all undeclared type names.
checkSignature :: Signature -> [TypeName]
checkSignature (types,(fname,params,res,doc):fundecls) =
  checkTypeLists types (res:params) ++ checkSignature (types,fundecls)
checkSignature (types,[]) = []

-- | Checks whether each name in the list of type names is declared.
-- The type declarations are the first argument, the list of types the second.
-- Returns a list of all misspelled type names (from the second list).
checkTypeLists :: [TypeDeclaration] -> [TypeName] -> [TypeName]
checkTypeLists types (typ:typs) 
  = if lookup typ types == Nothing then typ:problems else problems
    where problems = checkTypeLists types typs
checkTypeLists types [] = [] 

-----------------------
-- | Create a new, empty type environment
newTypeEnvironment :: TypeEnvironment
newTypeEnvironment = []

-- | Checks if all type names in an environment have been declared in the signature.
-- Returns a list of all misspelled type names.
checkTypeEnvironment :: Signature -> TypeEnvironment -> [TypeName]
checkTypeEnvironment sig@(types,fdecls) ((var,typ):tenv) 
  = if lookup typ types == Nothing then typ:problems else problems
    where problems = checkTypeEnvironment sig tenv
checkTypeEnvironment sig [] = []

-- | Checks that the type environment is a mapping from strings to types.
-- Generally, a type environment is an association list from strings to type names,
-- and may contain multiple types for each string.
-- A mapping gives each string a unique type. 
-- Thus we need to check that each string appears only once in the environment.
checkUniqueMap :: TypeEnvironment -> [String]
checkUniqueMap ((str,typ):utm) = case lookup str utm of
  Nothing -> checkUniqueMap utm
  Just typ' -> str:checkUniqueMap utm
checkUniqueMap [] = []


-----------------------
-- | Checks that a function model provides a semantics for all functions declared in a signature.
-- Turns each function declaration in the signature into a call of the corresponding function,
-- in order to check the function model recognises a function and computes a result.
-- Uses a test data function which maps a parameter list to a corresponding list of values.
checkFunModel :: Signature -> FunModel valuedomain -> TestData valuedomain -> [valuedomain]
checkFunModel (types,(fname,params,res,doc):fundecls) funmodel testfun
  = funmodel fname (testfun params)
    :checkFunModel (types,fundecls) funmodel testfun
checkFunModel (types,[]) funmodel testfun = []

-----------------------
-- | Unit test for consistency checking signatures, type environments and function models:
-- checkSignature, checkFunModel and checkEnvironment on an integer domain. 
unittestPam8Signature = do
  print $ "-- unittestPam8Signature --"
  let sig1 = ([],("f",["X","Y","Z"],"Int","doc f X Y Z"):[])::Signature
  let sig2 = ([("Integer","")],[("f",["Integer","Integer"],"Integer","doc f")])::Signature
  let sig3 = ([("X",""),("Z",""),("T","")],snd sig1)::Signature
  let vars = [("x","Integer"),("y","Integer")]::TypeEnvironment
  let fmod "f" [x,y] = x+y ; fmod "f" [x,y,z] = x*y*z
  let ch1 = checkSignature sig1 == ["Int","X","Y","Z"]
  let ch2 = checkSignature sig2 == []
  let ch3 = checkSignature sig3 == ["Int","Y"]
  let chu1 = checkUniqueMap vars == []
  let chu2 = checkUniqueMap (("x","X"):vars) == ["x"]
  let chv1 = checkTypeEnvironment sig1 vars == ["Integer","Integer"]
  let chv2 = checkTypeEnvironment sig2 vars == []
  let testfun params = [10..9+length params]
  let chfm1 = checkFunModel sig1 fmod testfun == [1320]
  let chfm2 = checkFunModel sig2 fmod testfun == [21]
  print $ if ch1 && ch2 && ch3 && chu1 && chu2 && chv1 && chv2 && chfm1 && chfm2 
    then "OK" 
    else "Not OK"
