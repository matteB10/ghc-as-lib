module Solution43 where

mylength :: [a] -> Int
mylength =
  {-# F A lot of Haskell programmers prefer to work in
        point-free style. Point-free style means avoiding
        the explicit naming of variables. Computations
        are just composed together using application and
        function composition. Another word for this style of
        programming is concatenative programming.
  #-} {-# F @foldrFeedback #-} foldr
        {-# F To keep the solution point-free we have to come up with a foldr
              step function which does not use lambda abstractions. 
        #-} {-# F An infix operator can be made prefix by surrounding them with parenthesis. #-} 
        {-# F Function const can be used to accept but ignore one argument #-} ((+) . (const 1)) 0

{-# RULES "mapfusion"    forall f g xs.  map f (map g xs) = map (f . g) xs #-}
{-# RULES "$/app"   forall a. forall (f :: a -> a) (g :: a -> a) (x :: a).  f $ g x = f (g x) #-}
{-# RULES "map/listcomprehension"   forall f xs.  map f xs = [f x | x <- xs] #-}
{-# RULES "map/concatMap"   forall f xs.  concat (map f xs)  = concatMap f xs #-}