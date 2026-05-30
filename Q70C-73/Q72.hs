--Q72, construct a bottom up sequence of nodes in a multi-way tree

data Tree a = Node a [(Tree a)]

bottomUp :: (Tree a) -> [a]
bottomUp (Node val []) = [val]
bottomUp (Node val list) =  (concat (map (bottomUp) list)) ++ [val]