module Test1 where 


data Expr = Num Double 
  | MonoOp MonoFunc Expr 
  | BinOp BinFunc Expr Expr
  | Var
--deriving (Eq)

data MonoFunc = Sin | Cos
--deriving (Eq, Show)

data BinFunc = Add | Mul
-- deriving (Eq, Show) 


{- eval :: Expr -> Double -> Double
eval  Var x             = x
eval (Num n) _          = n
eval _ _                = _ 
 -}