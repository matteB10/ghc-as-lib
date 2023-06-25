module Solution42 where

mylength :: [a] -> Int

mylength = {-# F @foldrFeedback #-}
    foldr {-# F How does the length of a list change when an element is added? #-} (\_ -> (+1)) {-# F The length of an empty list. #-} 0

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}