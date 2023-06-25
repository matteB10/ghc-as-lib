module Solution81 where

compress :: Eq a => [a] -> [a]

compress (x : y : xs) =
    {-# F An if expression can used as any normal expression in Haskell. Where in other
        (imperative) languages an if-statement influences the program flow, the
        Haskell if statement just chooses between two values.
    #-} (if x == y then [] else [x]) ++ compress (y : xs)
compress x = x
    {-# Clever pattern-matching, since it is last after (x:y:ys) it accounts for both [x] and [] #-}

{-
     Very similar duplicates
{-# DESC @pattern-match on the empty @list, a singleton list, and lists with at least two elements. #-}

compress []        =  []
compress [x]       =  [x]
compress (x:y:ys)  =  if x==y then compress (y:ys) else x:compress (y:ys)



{-# DESC @pattern-match on the empty @list, a singleton list, and lists with at least two elements. #-}

compress []        =  []
compress [x]       =  [x]
compress (x:y:ys)  =  if x==y then compress (x:ys) else x:compress (y:ys)
-}

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}