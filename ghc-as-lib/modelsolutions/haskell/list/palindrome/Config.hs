{-# DESC
Write a function that finds out whether a list is a palindrome:

palindrome :: Eq a => [a] -> Bool

A palindrome can be read forward or backward.

For example:

> palindrome "madam"
True
#-}

module Config where

import Test.QuickCheck

properties :: [(String -> Bool) -> Property]
properties = [propModel]

propModel f = forAll (oneof [arbitrary, genPalindrome]) $ 
    \xs -> f xs == (reverse xs == xs)

genPalindrome :: Arbitrary a => Gen [a]
genPalindrome = arbitrary >>= \xs -> return (xs ++ reverse xs)
