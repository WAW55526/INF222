module ExampleProcedures where

import BIPL3AST
import BIPL3MetaTypes
import PIPLMeta

-- procedure silly_add(upd x : integer, upd y : integer)
-- begin
--   while (x > 0) begin
--     x := x - 1;
--     y := y + 1;
--   end;
-- end;
--
-- procedure main(upd num : integer)
-- begin
--   silly_add(num, num);
-- end;
sillyAddProgram :: Program
sillyAddProgram =
  [ Proc "silly_add" [ (Upd, ("x", IntegerType)), (Upd, ("y", IntegerType)) ] []
    (While (CallFun "gt" [VarExp "x", IL 0])
         (Sequence [ Assign "x" (CallFun "plus" [VarExp "x", IL (-1)])
                   , Assign "y" (CallFun "plus" [VarExp "y", IL 1])
                   ]
         ))
  , Proc "main" [(Upd, ("num", IntegerType))] []
    (CallProc "silly_add" [ VarExp "num", VarExp "num" ])
  ]

-- procedure fib(obs n : integer, out r : integer)
-- var r1 : integer;
-- var r2 : integer;
-- begin
--   if (n <= 1)
--     r := 1;
--   else begin
--     fib(n - 1, r1);
--     fib(n - 2, r2);
--     r := r1 + r2;
--   end;
-- end;
fibonacciProgram :: Program
fibonacciProgram =
  [ Proc { procName = "fib"
         , procParams = [ (Obs, ("n", IntegerType)), (Out, ("result", IntegerType)) ]
         , procLocals = [ ("r1", IntegerType), ("r2", IntegerType) ]
         , procBody =
  (Sequence [ IfStmt (CallFun "le" [VarExp "n", IL 1])
              (Assign "result" (IL 1))
              (Sequence [ CallProc "fib" [ CallFun "plus" [VarExp "n", IL (-2)], VarExp "r1" ]
                        , CallProc "fib" [ CallFun "plus" [VarExp "n", IL (-1)], VarExp "r2" ]
                        , Assign "result" (CallFun "plus" [VarExp "r1", VarExp "r2"])
                        ])
            ])
         }
  ]

-- | Procedure with a malformed procedure call.
-- procedure badCall(obs x : integer)
-- begin
--   if (x <= 10)
--     badCall(True);
-- end;
badCallProgram :: Program
badCallProgram =
  [ Proc "badCall" [ (Obs, ("x", IntegerType)) ] []
    (IfStmt (CallFun "le" [VarExp "x", IL 10])
         (CallProc "badCall" [ BL True ])
         (Sequence []) -- Empty else branch
    )
  ]

-- | Procedure with aliasing issue.
-- procedure aliasTest1(obs a : integer,
--                      obs b : integer,
--                      upd c : integer,
--                      obs d : integer,
--                      out e : integer,
--                      obs f : integer,
--                      upd g : integer)
-- begin
--   while (a > f) begin
--     aliasTest1(a - 1, a, a, b, c, c, b);
--     g = g + 1;
--   end;
--   e = g + a;
--   aliasTest1(c, c, g, a, e, b, e);
-- end;
aliasTest1Program :: Program
aliasTest1Program =
  [ Proc "aliasTest1" (map (\(mode, name) -> (mode, (name, IntegerType)))
  [ (Obs, "a")
  , (Obs, "b")
  , (Upd, "c")
  , (Obs, "d")
  , (Out, "e")
  , (Obs, "f")
  , (Upd, "g")
  ])
  []
  (Sequence
  [ (While (CallFun "gt" [ VarExp "a", VarExp "f" ])
         (Sequence
         [ CallProc "aliasTest1" [ CallFun "plus" [ VarExp "a", IL (-1) ]
                                 , VarExp "a"
                                 , VarExp "a"
                                 , VarExp "b"
                                 , VarExp "c"
                                 , VarExp "c"
                                 , VarExp "b"
                                 ]
         , Assign "g" (CallFun "plus" [ VarExp "g", IL 1 ])
         ]))
  , Assign "e" (CallFun "plus" [ VarExp "g", VarExp "a"])
  , CallProc "aliasTest1" [ VarExp "c"
                          , VarExp "c"
                          , VarExp "g"
                          , VarExp "a"
                          , VarExp "e"
                          , VarExp "b"
                          , VarExp "e"
                          ]
  ])
  ]
