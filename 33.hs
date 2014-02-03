-- Problem 33
-- (*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.
--
-- Example:
--
-- * (coprime 35 64)
-- T
-- Example in Haskell:
--
-- * coprime 35 64
-- True

myGCD :: Int -> Int -> Int
myGCD a b
    | a `mod` b == 0 = b
    | a < b          = myGCD b a
    | otherwise      = myGCD b (a `mod` b)

coprime :: Int -> Int -> Bool

coprime a = (== 1) . myGCD a
