{-# DESC
  Write a function that reverses a list: myreverse :: [a] -> [a]. For example:

    > myreverse "A man, a plan, a canal, panama!"
    "!amanap ,lanac a ,nalp a ,nam A"

    > myreverse [1,2,3,4]
    [4,3,2,1]
#-}

module Config where

import Test.QuickCheck
{-import qualified ReverseFoldl as Foldl-}

properties :: [([Int] -> [Int]) -> Property]
properties = [propModel, propElems {- , propFoldl-}]

propElems f = counterexample "Input and output list do not contain the same elements" $
    \xs -> and $ map ((flip elem) xs) (f xs)

propModel f = counterexample "You have not reversed correctly" $
    \xs -> f xs == reverse xs

{-propFoldl = counterexample "Bla" $-}
  {-forAll arbitrary $ \xs -> f xs == Foldl.myreverse xs-}
