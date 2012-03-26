-- P12> decodeModified 
--   [Multiple 4 'a',Single 'b',Multiple 2 'c',
--     Multiple 2 'a',Single 'd',Multiple 4 'e']
-- "aaaabccaadeeee"

data Encoded a = Single a | Multiple Int a deriving (Show)


decodeModified :: (Eq a) => [Encoded a] -> [a]

decodeModified [] = []

decodeModified (x:xs) = (decode x) ++ decodeModified xs
    where   decode (Single a) = [a]
            decode (Multiple i a) = replicate i a


decodeModified' :: (Eq a) => [Encoded a] -> [a]

decodeModified' = foldl decode []
    where   decode acc (Single a) = acc ++ [a]
            decode acc (Multiple i a) = acc ++ replicate i a
