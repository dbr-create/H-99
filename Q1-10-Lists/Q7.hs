-- Q7, flattening a nested list

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten (List []) = []
flatten (List (x : xs)) = flatten x ++ flatten (List xs)
flatten (Elem a) = [a]
