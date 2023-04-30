{-# DESC
Write a function that eliminates consecutive duplicates of list elements.

compress :: Eq a => [a] -> [a]

If a list contains repeated elements they should be replaced with a single copy of the element. The order of the elements should not be changed.

For example:

> compress [2,2,2,1,2,2,3,3,3]
[2,1,2,3]
#-}

module Config where

import Test.QuickCheck

properties :: [([Int] -> [Int]) -> Property]
properties = [propUnique, propElem, propModel]

propUnique f = 
  counterexample "There are equals neighbors in the result" $
  forAll arbitrary $ \xs -> let un (x:y:ys) = x /= y && un (y:ys)
                                un x        = True
                            in un (f xs)

propElem f = counterexample "Not every element of the input is part of the result" $
  forAll arbitrary $ \xs -> and $ map ((flip elem) (f xs)) xs

propModel f = counterexample "Every required element is present, although not enough times or not in the right location" $
  forAll arbitrary $ \xs -> let modelS []     = []
                                modelS (y:ys) = y : (modelS $ dropWhile (== y) ys)
                            in f xs == modelS xs
