reverse' :: [a] -> [a]

reverse' [x] = [x]
reverse' (x:xs) = reverse' xs ++ [x]

reverse'' :: [a] -> [a]
reverse'' = foldl (\result a -> ([a]++result)) []
