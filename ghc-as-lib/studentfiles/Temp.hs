module Temp where

dupli [] = []
{-# RULES
    "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs
#-}

{-# RULES
    "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x)    
#-}


{-# RULES
    "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] 
#-}

{-# RULES
    "concatmap/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs 
#-}

{-# RULES "concat.map/concatMap" forall f.  concat . map f = concatMap f #-}


{-# RULES
    "lambda/pointfree" forall a. forall (f :: a -> a) (g :: a -> a) (xs :: [a]). map (\z -> f (g z)) xs = map (f . g) xs   
#-}
