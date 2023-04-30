{-# OPTIONS_GHC -Wno-typed-holes #-}
{-# OPTIONS_GHC -Wall #-}
module Temp2 where 


{- palindrome :: Eq a => [a] -> Bool
palindrome []  = True
palindrome [_] = True
palindrome xs  = (head xs) == (last xs) && (palindrome $ init $ tail xs) -}

--elementat :: [a] -> Int -> a 

{- elementat (x:_) 1  = x
elementat (_:xs) k = elementat xs (k - 1) -}

--elementat (x:xs) 1 = x
{- 
dropevery :: [a] -> Int -> [a]
dropevery [] _ = []
dropevery xs n = take (n-1) xs ++ dropevery (drop n xs) n -}

{- myconcat :: [[a]] -> [a] 
myconcat xs = foldr (++) [] xs -}

pack :: [a] -> [[a]]
pack [] = [[]]
pack [x] = [[x]]
pack xs = (head xs : tail xs) : [xs] 