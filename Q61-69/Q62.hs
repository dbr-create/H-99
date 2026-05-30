-- Q62, collect the internal nodes of a bianry tree in a list

data Tree a = Empty | Branch a (Tree a) (Tree a)

internals :: Tree a -> [a]
internals Empty = []
internals (Branch var Empty Empty) = []
internals (Branch var r l) = [var] ++ internals l ++ internals r