module Solution173 where

split :: [a] -> Int -> ([a], [a])
{-# DESC Recurse over both the @list and the integer. #-}

split [] _ = ([], [])
split xs 0 = ([], xs)
split (x:xs) n =
   let (f,l) = split xs (n-1)
   in {-# F Note that we do not have a case for when the length of the list is lower than the index.
         Haskell allows this, but you should avoid such cases if possible.
  #-} (x : f, l)

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
