--Q73, dsiplay a tree as a list lisp style

data Tree a = Node a [(Tree a)]

display :: (Tree Char) -> String
display tree = drop 1 (displayHelper tree)

displayHelper :: (Tree Char) -> String
displayHelper (Node val []) =  " " ++ [val]
displayHelper (Node val list) = " (" ++ [val] ++ (foldr (++) "" (map (displayHelper) list)) ++ ")"

