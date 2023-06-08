module Nat where 


data Nat = Succ Nat | Zero 

isZero :: Nat -> Bool 
isZero Zero = True 
isZero _    = False 

type N = Nat 

