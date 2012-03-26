

compress :: String -> String

compress [] = []
compress [a] = [a]
compress (x:xs)
    | x == head xs = compress xs
    | otherwise = [x] ++ (compress xs)


-- beautiful
compress' []     = []
compress' (x:xs) = x : (compress' $ dropWhile (== x) xs)
