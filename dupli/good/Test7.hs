{-# OPTIONS_GHC -Wno-typed-holes #-}

module Test7 where


dupli xs = case xs of  
    [] -> []
    [x] -> [x,x]
    (x:xs) -> [x,x] ++ dupli xs 