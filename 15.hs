dupli' :: a -> Int -> [a]
dupli' a 1 = [a]
dupli' a n = a:(dupli' a (n-1))

repli :: [a] -> Int -> [a]

repli l 0 = []
repli l 1 = l

repli [x] n = dupli' x n
repli (x:xs) n = (dupli' x n) ++ (repli xs n)
