module Foldl where

fromBin :: [Int] -> Int
{-# DESC Implement fromBin using the @foldl @prelude function. #-}
{-# ETADECL 1 #-}

fromBin = 
  {-# F Define the fromBin function using @foldl. The operator should multiply 
        the intermediate result with two and add the value of the bit. This 
        solution therefore multiplies every bit in the list n-times by two while 
        summing the individual bits. #-}
  foldl op 0
    where 
      n `op` b = {-# F Multiply n by two and add b. #-} 2*n + b 

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
