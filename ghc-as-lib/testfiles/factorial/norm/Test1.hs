{-# OPTIONS_GHC -Wno-typed-holes #-}


module Test1 where 

factorial :: (Eq t, Num t) => t -> t 
factorial = f   

f 0 = 1 
f m = m * f (m - 1)  

