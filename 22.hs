-- 2 Problem 22
-- Create a list containing all integers within a given range.
--
-- Example in Haskell:
--
-- Prelude> range 4 9
-- [4,5,6,7,8,9]
--

range :: Int -> Int -> [Int]

range x y 
    | x == y = [x]
    | otherwise = x : range (x+1) y
