{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod1 where 

myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]