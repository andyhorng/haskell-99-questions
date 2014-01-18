-- 4 Problem 24
-- Lotto: Draw N different random numbers from the set 1..M.
--
-- Example:
--
-- * (rnd-select 6 49)
-- (23 1 17 33 21 37)
-- Example in Haskell:
--
-- Prelude System.Random>diff_select 6 49
-- Prelude System.Random>[23,1,17,33,21,37]

import System.Random

diff_select :: Int -> Int -> IO [Int]

diff_select n m = diff_select' n [1..m]

diff_select' n [] = return []
diff_select' 0 list = return []
diff_select' n list = do
    i <- randomRIO (0, ((length list)-1))
    let remaining = take i list ++ drop (i+1) list
    subresult <- diff_select' (n-1) remaining 
    return ((list !! i):subresult)
