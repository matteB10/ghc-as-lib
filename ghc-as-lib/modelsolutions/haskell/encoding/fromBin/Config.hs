{-# DESC
  Write a function that converts a list of bits to the corresponding integer
  value: fromBin :: [Int] -> Int. For example:

    > fromBin [1,0,1,0,1,0]
    42

    > fromBin [1,0,1]
    5
#-}

module Config where

import Data.List (unfoldr)
import Test.QuickCheck

properties :: [([Int] -> Int) -> Property]
properties = map withBits [propModel, propSound, propDivByTwo]
  where
    withBits prop = forAll genBin . prop

propModel f bs = counterexample msg (output == model)
  where
    output  =  f bs
    model   =  foldl (\n b -> 2*n + b) 0 bs
    msg     =  "Your implementation is incorrect for the " ++
               "following input: " ++ show bs ++ "\nWe expected " ++ 
               show model ++ ", but we got " ++ show output        

propDivByTwo f bs = bs /= [] ==> f (init bs) == f bs `div` 2

propSound f bs = counterexample msg (bs == toBin (f bs))
  where 
    msg = "Converting back results in a different list of bits"

toBin :: Int -> [Int]
toBin = reverse . unfoldr f 
  where
    f n | n > 0     = let (n', b) = n `divMod` 2 in Just (b, n')
        | otherwise = Nothing

genBin :: Gen [Int]
genBin = let max = length (toBin maxBound) in do
    bs <- arbitrary >>=  return . map (\b -> if b then 1 else 0)
    return $ take max $ dropWhile (== 0) bs
