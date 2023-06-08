--{-# LANGUAGE ExtendedDefaultRules#-}
--{-# LANGUAGE NoMonomorphismRestriction #-}
module Temp2 where 
import Prelude hiding (length)

--fromBin :: Num t => [t] -> t
--fromBin :: (Foldable f, Num a) => f a -> a
--fromBin = foldl (\ n b -> 2*n + b) 0 
 
fromBin = foldl op 0 
    where op n b = 2*n + b 

{- isZero :: (Eq t, Num t) => t -> Bool 
isZero x = not (x /= 0) -}

{- length :: [a] -> Integer 
length [] = 0 -}
--length (x:xs) = _ -- 1 + length xs 

--length :: [a] -> Integer 

-- length :: [a] -> Integer
{- length :: [a] -> Integer
length = let len :: [a] -> Integer 
             len [] = 0
             len (x:xs) = 1 + len xs 
         in len  
   -}

{- l [] = 0 
l (x:xs) = 1 + l xs  -}

{- compress []    =  []
compress [x]   =  [x]
compress (x:y:xs)  | x==y = compress (x:xs)
                   | otherwise = x:y:compress xs  -}

{- elementat :: [a] -> Int -> a 
elementat ( _ :  _) i =  _ -}


{- tf :: Int -> Int 
tf 0 = 1
tf n = let g 1 = 1
           g n = f n 
           f 0 = 1 
           f n = g n 
        in g n  -}

{- hello :: Nat -> [Nat] 
hello Zero = map f []
hello (Succ n) = map g [] -}

{- length' :: [a] -> Int 
length' = foldl fun 0
fun x _ = x + 1  -}

{- elementat :: [a] -> Int -> a
elementat (x:xs) 0 = x
elementat (x:xs) y = elementat xs (y - 1)
 -}

{-
isOne :: Integer -> Bool 
isOne x = x == 1  -}
{- mylast [x] = x
mylast (x:xs)=x -}
{- factorial 0 = 1 
factorial n = n * factorial (n-1) -}

{- mylast :: [a] -> a 
mylast (x:xs) = mylast xs    -}

{- compress :: Eq a => [a] -> [a]
compress (x:y:xs) | x == y = compress (y:xs)
                  | True = x : compress (y:xs) -}
{- dupli :: [a] -> [a]
dupli xs = concatMap (replicate 2) xs  -}   


{- elementat :: [a] -> Int -> a
elementat list i = _  -}

-- den sista ontrACK som ej matchas
--elementat (x:xs) 1 = x
{- mylast :: [a] -> a
mylast [x] = x
mylast (_:xs) = mylast xs -}

{- palindrome :: Eq a => [a] -> Bool
palindrome []  = True
palindrome [_] = True
palindrome xs  = (head xs) == (last xs) && (palindrome $ init $ tail xs) -}

--elementat :: [a] -> Int -> a 

{- elementat (x:_) 1  = x
elementat (_:xs) k = elementat xs (k - 1) -}

--elementat (x:xs) 1 = x
{- dropevery :: [a] -> Int -> [a]
dropevery xs n = take (n-1) xs ++ dropevery (drop n xs) n -}
{- 
dropevery :: [a] -> Int -> [a]
dropevery [] _ = []
dropevery xs n = take (n-1) xs ++ dropevery (drop n xs) n -}

{- myconcat :: [[a]] -> [a] 
myconcat xs = foldr (++) [] xs -}

{- pack :: [a] -> [[a]]
pack [] = [[]]
pack [x] = [[x]]
pack _ = _  -}
--pack xs = (head xs : tail xs) : [xs] 

--dupli  = concat . map (replicate 2)

{- mylast = \ds -> case ds of 
        [] -> error "pattern error"
        (x:xs) -> case xs of 
            [] -> x 
            (_:_) -> mylast xs   -}