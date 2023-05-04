module DupliExpRec1 where


dupli :: [a] -> [a]
{-# DESC Use explicit @recursion (1st variant). #-}

{-# FC 
  Doing explicit recursion means we have to explicitly @pattern-match on the argument
  and decide what to do. For a @list this means looking at the nil and the cons case, 
  and apply the function recursively in the cons case.
#-}
dupli [] = []
dupli (x:xs) = {-# F Duplicate the head of the list, and call the function dupli recursively #-} x : x : {-# F recursive call of dupli. #-} dupli xs

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
