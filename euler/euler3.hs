-- Largest prime factor
-- Problem 3
-- The prime factors of 13195 are 5, 7, 13 and 29.
--
-- What is the largest prime factor of the number 600851475143 ?

main = do
    print (getMax)

n = 600851475143
nSquared = (isqrt n)

getMax =
    head [x | x <- [nSquared, nSquared - 1..2], n `mod` x == 0 && isPrime x]

isPrime v =
    v > 1 && not (even v) && (length [x | x <- [2..(isqrt v)], v `mod` x == 0]) == 0

-- from http://lpaste.net/46245
isqrt :: Integer -> Integer
isqrt n | n >= 0   = go 0 (n+1) where
    -- invariant: lo*lo <= n < hi*hi
    go lo hi
        | lo == hi-1 = lo
        | m*m <= n   = go m hi
        | n < m*m    = go lo m
            where
                m = div (hi + lo) 2
