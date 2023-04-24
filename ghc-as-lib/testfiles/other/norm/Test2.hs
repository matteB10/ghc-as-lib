module Test2 where 

{-# RULES
    "lambda/pointfree" forall a. forall (f :: a -> a) (g :: a -> a) (xs :: [a]). map (\z -> f (g z)) xs = map (f . g) xs   
#-}


addAndMult :: Int -> [Int] -> [Int]
addAndMult n xs = map (\x -> (2*x) + 2) xs 