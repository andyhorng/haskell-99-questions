-- 10 Problem 20
-- (*) Remove the K'th element from a list.
--
-- Example in Prolog:
--
-- ?- remove_at(X,[a,b,c,d],2,R).
-- X = b
-- R = [a,c,d]
-- Example in Lisp:
--
-- * (remove-at '(a b c d) 2)
-- (A C D)
-- (Note that this only returns the residue list, while the Prolog version also returns the deleted element.)
--
-- Example in Haskell:
--
-- *Main> removeAt 2 "abcd"
-- ('b',"acd")
--

removeAt :: Int -> [a] -> (a, [a])
removeAt n list = (list !! (n-1), [ list !! (x-1) | x <- [1..(length list)], x /= n])


--- other version using recursion and Maybe

removeAt' :: Int -> [a] -> (Maybe a, [a])
removeAt' _ [] = (Nothing, [])
removeAt' 1 (x:xs) = (Just x, xs)
removeAt' k (x:xs) = let (a, r) = removeAt' (k - 1) xs in (a, x:r)
