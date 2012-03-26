
myLast :: [a] -> a

myLast [] = []
myLast [a] = a
myLast (x:xs) = myLast xs

