module Lab1 where


-- recursive power function
-- n^k
power :: Integer -> Integer -> Integer
power n k
  | k < 0 = error "power: negative argument"
power n 0 = 1
power n k = n * power n (k-1)

 -- A

stepsPower n k = k


-- B
power1 :: Integer -> Integer -> Integer
power1 n k = product (replicate (fromInteger k) (fromInteger n))


-- C

power2 :: Integer -> Integer -> Integer
power2 n 0 = 1
power2 n k | even k    = power2 (n*n) (k `div` 2)
           | otherwise = n * power2 n (k-1)


-- D

-- a
test_cases = [(0,0), (5,1), (2,3), (0,5), (3,3), (1,100)]

-- b
prop_powers :: Integer -> Integer -> Bool
prop_powers n k = power n k == power1 n k &&  power1 n k == power2 n k


-- c          

powerTest :: [(Integer,Integer)] -> Bool
powerTest xs = and [prop_powers n k | (n,k) <- xs] 

-- d
prop_powers' :: Integer -> Integer -> Bool
prop_powers' n k | n >= 0 && k >= 0 = power n k == power1 n k &&  power1 n k == power2 n k
                 | otherwise = True


