myButLast :: [a] -> a
myButLast [a, b] = a
myButLast (_:xs) = myButLast xs
