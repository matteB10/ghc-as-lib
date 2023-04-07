{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Test1 where 

myreverse _      = []
myreverse (x:xs) = myreverse xs ++ [x]