elementAt :: [a] -> Int -> a
elementAt a b = a !! (b - 1)

elementAt' (x:_) 1 = x
elementAt' (x:xs) ix = elementAt' xs (ix-1)

