-- 5 Problem 25
-- Generate a random permutation of the elements of a list.
--
-- Example:
--
-- * (rnd-permu '(a b c d e f))
-- (B A D C E F)
-- Example in Haskell:
--
-- Prelude>rnd_permu "abcdef"
-- Prelude>"badcef"
import System.Random (randomRIO)

rndPermu :: Show a => [a] -> IO [a]
rndPermu [] = return []
rndPermu [x] = return [x]

rndPermu list = loop (length(list)-1) list onePermu

onePermu list = do
    i <- randomRIO (0, (length list) - 1)
    j <- randomRIO (0, (length list) - 1)
    return $ swap list i j
    where
        swap list@(x:xs) i j
            | i > j     = swap list j i
            | i == j    = list
            | i == 0    = (list !! j):(drop 1 $ take j list) ++ x:(drop (j+1) list)
            | otherwise = x:(swap xs (i-1) (j-1))

loop :: Int -> [a] -> ([a] -> IO [a]) -> IO [a]
loop 0 list action = action list
loop i list action = do 
    new <- action list
    loop (i-1) new action
