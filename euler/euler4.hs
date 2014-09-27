-- Largest palindrome product
-- Problem 4
-- A palindromic number reads the same both ways.
-- The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
--
-- Find the largest palindrome made from the product of two 3-digit numbers.

main = do
    print (findMaxPal 111 0)

findMaxPal n maxPal =
    if n <= 999 then
        findMaxPal (n + 1) (compute n maxPal)
    else
        maxPal

compute n maxPal =
    maximum (maxPal : [x * n | x <- [111..999], isPalindrome(x * n) &&  x * n > maxPal])

isPalindrome val =
    reverse(show(val)) == show(val)
