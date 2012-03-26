-- * (my-flatten '(a (b (c d) e)))
-- (A B C D E)


data NestedList a = Elem a | List [NestedList a]

flatten :: (NestedList a) -> [a]

flatten (Elem a) = [a]
flatten (List (x:xs)) = (flatten x) ++ (flatten (List xs))
flatten (List []) = []


flatten' :: (NestedList a) -> [a]
flatten' (Elem a) = [a]
flatten' (List l) = concatMap flatten' l
