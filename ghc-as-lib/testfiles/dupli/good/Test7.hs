{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test7 where


dupli :: [a] -> [a]
dupli xs = case xs of  
    [] -> []
    [x] -> [x,x]
    (x:xs) -> [x,x] ++ dupli xs