module ExampleExprs where

import BIPL3AST

-- plus(1, 2)
e1 = CallFun "plus" [IL 1, IL 2]

-- uminus(plus(1, 2))
e2 = CallFun "uminus" [e1]

-- or(false, plus(1, 2))
e3 = CallFun "or" [BL False, e1]

-- x + (-x)
e4 = CallFun "plus" [VarExp "x", CallFun "uminus" [VarExp "x"]]

-- x + y
e5 = CallFun "plus" [IL 10, VarExp "y"]
