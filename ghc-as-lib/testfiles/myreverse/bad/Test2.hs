{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test2 where 

myreverse (x:xs) = myreverse xs ++ [x]