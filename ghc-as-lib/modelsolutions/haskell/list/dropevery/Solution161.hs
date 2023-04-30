module Solution161 where

dropevery :: [a] -> Int -> [a]
{-# DESC Recurse over both the @list and the integer argument. #-}

{-# FC We need a helper function to do the actual calculation, we can not give a 
       directly @recursive definition. The helper function can be used to `remember' the value of n
       and use it to restart the recursion over the integer.
#-}
dropevery xs n = helper xs n
    where helper [] _ = []
          helper (y:ys) 1 = helper ys n
          helper (y:ys) k = y : helper ys (k-1)

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
