--{-# OPTIONS_GHC -fplugin=Splint  #-}
module Test10 where 


palindrome :: Eq a => [a] -> Bool 
palindrome xs = if xs /= reverse xs then False else True   


{- palindrome :: Eq a => [a] -> Bool
palindrome [] = True
palindrome xs = if zs == xs then True else False
    where zs = reverse xs -}