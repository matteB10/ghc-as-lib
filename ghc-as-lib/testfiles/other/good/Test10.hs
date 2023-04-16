--{-# OPTIONS_GHC -fplugin=Splint  #-}
module Test10 where 


palindrome :: Eq a => [a] -> Bool 
palindrome xs = reverse xs == xs 

{- palindrome :: Eq a => [a] -> Bool
palindrome [] = True
palindrome xs = if zs == xs then True else False
    where zs = reverse xs -}