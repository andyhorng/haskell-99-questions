slice :: [a] -> Int -> Int -> [a]
slice list i j = [ list !! (x-1) | x <- [i..j]]
