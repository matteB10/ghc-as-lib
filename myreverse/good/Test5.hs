{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module Test5 where


myreverse (x:xs) = myreverse xs ++ [x]
myreverse [] = []
myreverse _   = []