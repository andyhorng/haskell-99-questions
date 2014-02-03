-- Problem 31
-- (**) Determine whether a given integer number is prime.
--
-- Example:
--
-- * (is-prime 7)
-- T
-- Example in Haskell:
--
-- P31> isPrime 7
-- True
--

isPrime :: Int -> Bool

isPrime 2 = True
isPrime 3 = True
isPrime n = 
    not $ any ((== 0).mod n) [x | x <- [2..n], (x*x) <= n]
