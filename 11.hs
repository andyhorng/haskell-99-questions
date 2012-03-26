-- * (encode-modified '(a a a a b c c a a d e e e e))
-- ((4 A) B (2 C) (2 A) D (4 E))
--
data Encoded a = Single a | Multiple Int a deriving (Show)

encodeModified :: (Eq a) => [a] -> [Encoded a]

add :: Encoded a -> Encoded a
add (Single a) = Multiple 2 a
add (Multiple i a) = Multiple (i+1) a

encodeModified [] = []
encodeModified [x] = [Single x]
encodeModified (x:xs) 
    | x == head xs = (add $ head $ encodeModified xs) : (tail $ encodeModified xs)
    | otherwise  = (Single x) : (encodeModified xs)
