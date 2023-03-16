{-# OPTIONS_GHC -Wno-typed-holes #-}
module Mod2 where 

myreverse (x:xs) = myreverse xs ++ [x]
myreverse [] = []