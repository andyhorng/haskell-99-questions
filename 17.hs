split :: [a] -> Int -> ([a], [a])

split [] _ = ([], [])

split l@(x:xs) n 
    | n > 0  = (a ++ [b], c)
    | otherwise = ([], l)
    where
        (a, (b:c)) = split l (n-1)
