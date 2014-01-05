-- 9 Problem 19
-- (**) Rotate a list N places to the left.
--
-- Hint: Use the predefined functions length and (++).
--
-- Examples:
--
-- * (rotate '(a b c d e f g h) 3)
-- (D E F G H A B C)
--
-- * (rotate '(a b c d e f g h) -2)
-- (G H A B C D E F)
-- Examples in Haskell:
--
-- *Main> rotate ['a','b','c','d','e','f','g','h'] 3
-- "defghabc"
--  
--  *Main> rotate ['a','b','c','d','e','f','g','h'] (-2)
--  "ghabcdef"
--

rotate :: [a] -> Int -> [a]

rotate list 0 = list
rotate l@(x:xs) n = rotate (xs ++ [x]) offset
    where offset = if n > 0 then n - 1 else (length l) + n - 1
