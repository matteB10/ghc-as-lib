{-# DESC
Write the function identity that creates an identity matrix of a given size.

For example:

> identity 3
[ [1,0,0], [0,1,0], [0,0,1] ]
#-} 

module Config where

import Control.Monad
import Data.List hiding (transpose)
import Test.QuickCheck

properties :: [(Int -> [[Int]]) -> Property]
properties = [ propColCount, propRowCount, propColZeros, propColOne
             , propTranspose, propMultiply ]

forSmall = forAll (choose (1, 100))

feedback = counterexample

propRowCount f =
    feedback "The result does not contain the right number of rows" $
      forSmall $ \i -> length (f i) == i

propColCount f = 
    counterexample "The result does not contains columns of the right length" $
      forSmall $ \i -> and $ map (\xs -> length xs == i) (f i)

propColZeros f = 
    counterexample "Every column should only contain zeros, except for one position" $
      forSmall $ \i' -> let i = i'+1 in
        and $ map (\xs -> maximum xs == 1 && minimum xs == 0 && sum xs == 1) $ f i

propColOne f = 
    counterexample "The columns do not have the digit 1 at the right position" $
      forSmall $ \i -> 
        and $ map (\(xs, c) -> 1 == xs !! (c - 1)) $ zip (f i) [1 .. i]

propTranspose f = 
    counterexample "The transpose of the identity matrix should be the identity again." $
      forSmall $ \i -> f i == transpose (f i)

transpose :: [[a]] -> [[a]]
transpose []             = []
transpose ([]     : xss) = transpose xss
transpose ((x:xs) : xss) = (x : [h | (h:_) <- xss]) 
                         : transpose (xs : [ t | (_:t) <- xss])    

propMultiply f = forAll matrix $ \m -> m *** (f $ length $ head m) == m

matrix :: Gen [[Int]]
matrix = do 
    n <- choose (1, 10)
    m <- choose (1, 10)
    replicateM n (replicateM m pos)

pos :: Gen Int
pos = arbitrary >>= return . (+1) . abs

(***) :: Num a => [[a]] -> [[a]] -> [[a]] 
a *** b = [[ sum $ zipWith (*) ar bc | bc <- (transpose b)] | ar <- a]

identity n = 
  let f = \i b -> if b > n   then [] else
                  if b == i  then 2 : f i ((+) b 1) 
                             else 0 : f i ((+) 1 b)
  in map (flip f 1) [1 .. n]
