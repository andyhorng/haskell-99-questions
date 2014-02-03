-- 6 Problem 26
-- (**) Generate the combinations of K distinct objects chosen from the N elements of a list
--
-- In how many ways can a committee of 3 be chosen from a group of 12 people? We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes the well-known binomial coefficients). For pure mathematicians, this result may be great. But we want to really generate all the possibilities in a list.
--
-- Example:
--
-- * (combinations 3 '(a b c d e f))
-- ((A B C) (A B D) (A B E) ... )
-- Example in Haskell:
--
-- > combinations 3 "abcdef"
-- ["abc","abd","abe",...]
--

combinations :: Int -> [a] -> [[a]]
combinations 1 xs = [[x] | x <- xs]
combinations n list@(x:xs) = 
    concat [map (e:) (combinations (n-1) (drop (ix+1) list)) | (ix, e) <- indexed]
    where indexed = zip [0..((length xs) - 1)] list
