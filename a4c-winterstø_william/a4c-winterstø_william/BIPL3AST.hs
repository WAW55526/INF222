-- | BIPL3AST: Abstract syntax for BIPL,
-- BTL3 extended with statements (including Assert)
-- @date 2022-02-10
-- @author Magne Haveraaen

module BIPL3AST where

-- | Expression abstract syntax: BTL with Integers and booleans
data Expr = 
    IL Integer -- integer literal
    | Plus Expr Expr
    | Mult Expr Expr
    | Uminus Expr
    | Div Expr Expr
    | Mod Expr Expr
    | BL Bool -- boolean literal
    | Or Expr Expr
    | And Expr Expr
    | Not Expr
    | Choice Expr Expr Expr
    | Equal Expr Expr -- equality between integers
    | Le Expr Expr -- less or equal ≤
    | VarExp Var -- a variable is an expression
    deriving (Show, Eq, Read)

-- | Statement abstract syntax: from L0602
data Stmt = 
    Assert Expr
    | Assign Var Expr
    | While Expr Stmt
    | IfStmt Expr Stmt Stmt
    | Sequence [ Stmt ]
    deriving (Show, Eq, Read)

-- | Variables are strings
type Var = String


-----------------------

-- | Procedure for Euclidean division. 
-- The quotient of x and y is assigned to q while the remainder of x and y is assigned to r.
{-
    procedure eucliddiv ( obs x,y:integer, out q,r:integer );
    begin
    q := 0;
    r := x;
    while y <= r do
        begin
        r := r - y;
        q := q + 1;
        end;
    end;
-}
eucliddiv_stmt :: Stmt
eucliddiv_stmt = Sequence [
  Assign "q" (IL 0),
  Assign "r" (VarExp "x"),
  While (Le (VarExp "y") (VarExp "r"))
    (Sequence [
      Assign "r" (Plus (VarExp "r") (Uminus (VarExp "y"))),
      Assign "q" (Plus (VarExp "q") (IL 1))
    ])
  ]

type XY = (Integer,Integer)
type QR = (Integer,Integer)
type XYQR = (XY,QR)
eucliddiv_problems :: [XY]
eucliddiv_problems = [(13,7),(49,7),(16,32),(32,16),(32,15),(0,9),(1,9),(8,9),(9,9),(10,9)]
eucliddiv_answers :: [XYQR]
eucliddiv_answers = map (\(x,y) -> ((x,y),quotRem x y)) eucliddiv_problems

--------------------------
-- | Computus
{-
    (** Computing Easter Day for year Y using "Anonymous Gregorian algorithm". *)
    procedure computus ( obs Y:integer; out month,day:integer ) ;
    var a,b,c,d,e,f,g,h,i,k,l,m,n,o:integer;
    begin
    a := Y mod 19;
    b := Y div 100;
    c := Y mod 100;
    d := b div 4;
    e := b mod 4;
    f := (b + 8) div 25;
    g := (b - f + 1) div 3;
    h := (19*a + b - d - g + 15) mod 30;
    i := c div 4;
    k := c mod 4;
    l := (32 + 2*e + 2*i - h - k) mod 7;
    m := (a + 11*h + 22*l) div 451;
    n := (h + l - 7*m + 114) div 31;
    o := (h + l - 7*m + 114) mod 31;
    month := n;
    day := o + 1;
    end ;
-}
computus_stmt :: Stmt
computus_stmt = Sequence [
    -- a := Y mod 19;
    Assign "x" (VarExp "Y"),
    Assign "y" (IL 19),
    eucliddiv_stmt,
    Assign "a" (VarExp "r"),
    -- b := Y div 100;
    -- c := Y mod 100;
    Assign "x" (VarExp "Y"),
    Assign "y" (IL 100),
    eucliddiv_stmt,
    Assign "b" (VarExp "q"),
    Assign "c" (VarExp "r"),
    -- d := b div 4;
    -- e := b mod 4;
    Assign "x" (VarExp "b"),
    Assign "y" (IL 4),
    eucliddiv_stmt,
    Assign "d" (VarExp "q"),
    Assign "e" (VarExp "r"),
    -- f := (b + 8) div 25;
    Assign "x" ((Plus (VarExp "b")(IL 8))),
    Assign "y" (IL 25),
    eucliddiv_stmt,
    Assign "f" (VarExp "q"),
    -- g := (b - f + 1) div 3;
    Assign "x" (Plus (Plus (VarExp "b")(Uminus (VarExp "f")))(IL 1)),
    Assign "y" (IL 3),
    eucliddiv_stmt,
    Assign "g" (VarExp "q"),
    -- h := (19*a + b - d - g + 15) mod 30;
    Assign "x" hexp,
    Assign "y" (IL 30),
    eucliddiv_stmt,
    Assign "h" (VarExp "r"),
    -- i := c div 4;
    -- k := c mod 4;
    Assign "x" (VarExp "c"),
    Assign "y" (IL 4),
    eucliddiv_stmt,
    Assign "i" (VarExp "q"),
    Assign "k" (VarExp "r"),
    -- l := (32 + 2*e + 2*i - h - k) mod 7;
    Assign "x" lexp,
    Assign "y" (IL 7),
    eucliddiv_stmt,
    Assign "l" (VarExp "r"),
    -- m := (a + 11*h + 22*l) div 451;
    Assign "x" (Plus (Plus (VarExp "a")(Mult (IL 11)(VarExp "h")))(Mult (IL 22)(VarExp "l"))),
    Assign "y" (IL 451),
    eucliddiv_stmt,
    Assign "m" (VarExp "q"),
    -- n := (h + l - 7*m + 114) div 31;
    -- o := (h + l - 7*m + 114) mod 31;
    Assign "x" (Plus (Plus (Plus (VarExp "h")(VarExp "l"))(Uminus (Mult (IL 7)(VarExp "m"))))(IL 114)),
    Assign "y" (IL 31),
    eucliddiv_stmt,
    Assign "n" (VarExp "q"),
    Assign "o" (VarExp "r"),
    -- month := n;
    -- day := o + 1;
    Assign "month" (VarExp "n"),
    Assign "day" (Plus (VarExp "o")(IL 1))
    ]
-- h := (19*a + b - d - g + 15) mod 30;
hexp :: Expr
hexp = (Plus(Plus (Plus (Plus (Mult (IL 19)(VarExp "a"))(VarExp "b"))(Uminus (VarExp "d"))) (Uminus (VarExp "g")))(IL 15))
-- l := (32 + 2*e + 2*i - h - k) mod 7;
lexp :: Expr
lexp = (Plus(Plus (Plus (Plus (IL 32)(Mult (IL 2)(VarExp "e")))(Mult (IL 2)(VarExp "i")))(Uminus (VarExp "h")))(Uminus (VarExp "k")))

type Date = (Integer, (Integer, Integer)) -- (Year,(Month,Day))
computus_answers :: [Date]
computus_answers = [(2022,(4,17)),(2021,(4,4)),(2020,(4,12)),(2019,(4,21)),(2011,(4,24)),(2008,(3,23)),(2038,(4,25))]
