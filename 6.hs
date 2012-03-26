isPalindrome :: (Eq a) => [a] -> Bool

isPalindrome [] = True
isPalindrome [a] = True
isPalindrome x = 
    if head x == last x then
        isPalindrome $ init . tail $ x
    else
        False

isPalindrome' list = (==) list $ reverse list
