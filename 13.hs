-- don't create sublist
data Encoded a = Single a | Multiple Int a deriving (Show)

encodeDirect :: (Eq a) => [a] -> [Encoded a]

-- "aaaaabbb"
--

encodeDirect = foldr direct []
    where 
        direct next [] = [Single next]
        direct next (current@(c:cs)) 
            | next == element c = (inc c):cs
            | otherwise = (Single next):current
        element c = case c of { (Single a) -> a; (Multiple i a) -> a }
        inc c = case c of
            (Single a) -> (Multiple 2 a)
            (Multiple i a) -> Multiple (i+1) a

