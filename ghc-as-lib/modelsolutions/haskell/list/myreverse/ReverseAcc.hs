module ReverseAcc where

myreverse :: [a] -> [a]
{-# DESC Introduce a helper function that uses an accumulating parameter. #-}

myreverse = reverse' []
  where
    reverse' acc [] = acc
    reverse' acc (x:xs) =
      {-# F Because the result of the recursive call to
            @name_reverse' is the result of the function itself,
            @name_reverse' is called tail-recursive.
            Functions of this form can be optimized by a compiler.
            Introducing accumulating parameters can help in making a function
            tail-recursive.
      #-} reverse' (x:acc) xs

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}
