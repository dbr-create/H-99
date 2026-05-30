-- Q62B, find all the node values at a given level


data Tree a = Empty | Branch a (Tree a) (Tree a)

atLevel :: Tree a -> Int -> [a]
atLevel Empty _ = []
atLevel (Branch var r l) 1 = [var]
atLevel (Branch var r l) n = atLevel l (n - 1) ++ atLevel r (n - 1)