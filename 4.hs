myLength :: [a] -> Int

myLength a = foldr (\_ len -> len + 1) 0 a

myLength' [] = 0
myLength' (_:xs) = myLength' xs + 1

-- 值得注意
myLength''   =  foldr (\_ -> (+1)) 0

myLength''' = sum . map (\x -> 1)
