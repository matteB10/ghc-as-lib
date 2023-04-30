{-# DESC
  We can represent a matrix using a list of lists, where each list represents the row of a matrix.
  The goal of this exercise is to calculate the transpose of such a matrix.

  You can assume the input matrix has at least one row and one column.

  For example:

  > transpose [ [1,2,3], [4,5,6], [7,8,9], [10,11,12] ]
  [ [1,4,7,10], [2,5,8,11], [3,6,9,12] ]
#-}

module Config where

import Test.QuickCheck
import Control.Monad

properties :: [([[Int]] -> [[Int]]) -> Property]
properties = [propRows, propCols, propContents]

propRows f = counterexample "The columns of the result should have the same length as the number of rows in the input" 
           $ forAll matrix 
           $ \m -> let cols = length m 
                   in and $ map (\ys -> length ys == cols) (transpose m)

propCols f = counterexample "The rows of the result should have the same length as the number of columns in the input" 
           $ forAll matrix 
           $ \m -> let rows = length (head m) 
                   in rows == length (transpose m)

propContents f = counterexample "The resulting rows are not in the right order" 
               $ forAll matrix 
               $ \m -> let model [r]     =  map (:[]) r 
                           model (r:rs)  =  zipWith (:) r (model rs) 
                       in model m == transpose m

matrix :: Gen [[Int]]
matrix = do 
    n <- choose (1, 10)
    m <- choose (1, 10)
    replicateM n (replicateM m pos)

pos :: Gen Int
pos = arbitrary >>= return . (+1) . abs

transpose [r]    = map (:[]) r
transpose (r:rs) = zipWith (:) r (transpose rs)

