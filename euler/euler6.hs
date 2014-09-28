-- The sum of the squares of the first ten natural numbers is,

-- 12 + 22 + ... + 102 = 385
-- The square of the sum of the first ten natural numbers is,

-- (1 + 2 + ... + 10)2 = 552 = 3025
-- Hence the difference between the sum of the squares of the
-- first ten natural numbers and the square of the sum is 3025 â 385 = 2640.

-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

main = do
    print(squareSum - sumSquares)

sumSquares =
    sum [x ^ 2 | x <- [1..100]]

squareSum =
    (sum [x | x <- [1..100]]) ^ 2
