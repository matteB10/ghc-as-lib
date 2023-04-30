module Solution41 where

mylength :: [a] -> Int
{-# DESC Use explicit @recursion. #-}

mylength []        =  0
mylength (_:xs)    =
  {-# F Although a solution using explicit recursion is correct,
        the solution can be written much shorter with help of @foldr.
  #-} (1 + mylength xs)

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
