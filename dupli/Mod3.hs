{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod3 where
    
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x,x])  

{- test :: a -> [a] -> [a]
test = (:) -}

{- f :: (Int -> Int -> Int) -> Int -> Int -> Int 
f g = g  -}