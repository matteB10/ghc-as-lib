{-# OPTIONS_GHC -Wno-typed-holes #-}
module Test6 where 

dupli :: [a] -> [a]
dupli [] = []
dupli xs = concatMap (\x -> [x,x]) xs 
