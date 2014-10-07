-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
--
-- What is the 10 001st prime number?

main = do
    print(getPrimeN 10001)

getPrimeN n =
    (2 : [x | x <- [3, 5..], isPrime x]) !! (n - 1)

isPrime x =
    all (cannotDivide x) [squared, (squared - 1)..2]
    where
        squared = isqrt x

cannotDivide x y =
    x `mod` y /= 0

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
