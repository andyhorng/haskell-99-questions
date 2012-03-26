
pack :: (Eq a) => [a] -> [[a]]

pack [] = []
pack list = (takeWhile (==first) list) : (pack $ dropWhile (==first) list)
    where first = head list


encode :: (Eq a) => [a] -> [(Int, a)]

encode = map code . pack 
    where code l = (length l, head l)


