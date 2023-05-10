module Mod1 where 

data Expr = Num Double 
  | MonoOp MonoFunc Expr 
  | BinOp BinFunc Expr Expr
  | Var
--deriving (Eq)

data MonoFunc = Sin | Cos
--deriving (Eq, Show)

data BinFunc = Add | Mul
--deriving (Eq, Show) 


eval :: Expr -> Double -> Double
eval  Var x             = x
eval (Num n) _          = n
eval (BinOp Add i j) x  = (eval i x) + (eval j x) 
eval (BinOp Mul i j) x  = (eval i x) * (eval j x)
eval (MonoOp Cos i) x   = cos (eval i x)
eval (MonoOp Sin i) x   = sin (eval i x)