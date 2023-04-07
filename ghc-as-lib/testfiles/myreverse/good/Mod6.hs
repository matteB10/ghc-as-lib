{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod6 where 

myreverse [] = []
myreverse (x:xs) = myreverse xs ++ [x]