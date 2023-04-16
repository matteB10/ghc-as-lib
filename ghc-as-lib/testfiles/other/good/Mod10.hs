module Mod10 where 


palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs
