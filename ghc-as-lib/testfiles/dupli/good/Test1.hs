--{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test1 where 

dupli :: [a] -> [a] 
dupli xs = concatMap hole xs
    where hole = _ 
