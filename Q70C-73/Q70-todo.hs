-- Q70, create a predicate to construct a multi-way tree from a string (and vice versa)

data Tree a = Node a [(Tree a)]
    deriving (Show, Eq)

stringToTree :: String -> (Tree Char)
stringToTree (curr:rest) | (foldr (&&) (True) (map (=='^') (rest))) == True = Node (curr) []
                         | otherwise = Node (curr) (map (stringToTree) (partition rest [] 0))


partition :: String -> String -> Int -> [String]
partition []  accumulator _ = [accumulator]
partition ('^':rest) accumulator depth = partition rest (accumulator ++ "^") (depth - 1)
partition (current:rest) accumulator depth | depth == 0 && accumulator == [] = (partition (rest) ([current]) (depth + 1))
                                           | depth == 0 = [accumulator] ++ (partition (rest) ([current]) (depth + 1))
                                           | otherwise = (partition rest (accumulator ++ [current]) (depth + 1))

treeToString :: (Tree Char) -> String
treeToString tree = treeToStringHelper 0 tree

treeToStringHelper :: Int -> (Tree Char) -> String
treeToStringHelper depth (Node val []) = [val] ++ replicate depth '^'
treeToStringHelper depth (Node val values) = [val] ++ concat (map (treeToStringHelper (depth + 1)) values)

                        
