{-# OPTIONS_GHC -Wno-typed-holes #-}


module Test1 where 

factorial :: (Eq t, Num t) => t -> t 
factorial = f 
-- {-# INLINE f  #-}  has no effect if GHC deems that there is no performance gain in inlining this function

--f :: Integer -> Integer 
-- without type signature, this is bounded as a NonRec with a letrec inside
f :: (Eq t, Num t) => t -> t
f 0 = 1 
f m = m * f (m - 1)  

