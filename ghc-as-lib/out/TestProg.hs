

{-# LANGUAGE LambdaCase #-}
-- set these options in Main (in the function comp) instead
--{-# OPTIONS_GHC -frefinement-level-hole-fits=2 #-}
--{-# OPTIONS_GHC -fdefer-typed-holes #-}

module TestProg where 


import Data.List (sort)

type List a = [a]


dupli :: List a -> List a
dupli x = concatMap (replicate 2) x 

dupli1 :: List a -> List a
dupli1 = concatMap (replicate 2)

dupli2 :: [a] -> [a]
dupli2 []      = []
dupli2 (ff:ffs) = [ff,ff] ++ dupli2 ffs

dupli3 :: [a] -> [a]
dupli3 = foldr (\x y -> [x,x]++y) []

dupli4 :: [a] -> [a]
dupli4 = foldr (\x y -> x:x:y) []

dupli5 :: [a] -> [a]
dupli5 = foldl (\x y -> y:y:x) []

dupli6 :: [a] -> [a]
dupli6 [] = []
dupli6 (x:xs) = dup x ++ dupli6 xs
    where dup x = [x,x]

dupli7 :: [b] -> [b]
dupli7 = \case
    []     -> []
    (x:xs) -> x:x:dupli7 _

-- in core, dupli7 should be equivalent to:

dupli8 :: [b] -> [b]
dupli8 [] = []
dupli8 (x:xs) = x:x:dupli8 xs 

dupli9 :: [a] -> [a]
dupli9 = concatMap (\x -> [x,x])

dupli10 :: [a] -> [a]
dupli10 = concat . map (replicate 2)

dupli11 :: [a] -> [a]
dupli11 = concat . map (\x -> [x,x])


-- testing that all type errors not defered 
--dupli12 :: [a] 
--dupli12 = dupli11 

