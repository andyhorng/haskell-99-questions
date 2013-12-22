dropEvery :: [a] -> Int -> [a]
dropEvery list n = [list !! (y-1) | y <- [x | x <- [1,2..l], x `mod` n /= 0]]
    where l = length list
