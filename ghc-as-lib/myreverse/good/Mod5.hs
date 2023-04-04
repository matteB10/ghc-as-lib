{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod5 where 

myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]