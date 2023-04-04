{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test1 where 

myreverse [] = []
myreverse (x:xs) = _ ++ [x]