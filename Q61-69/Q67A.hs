-- Q67A, convert from tree to string and vice versa

data Tree a = Empty | Branch a (Tree a) (Tree a)
    deriving (Show)

treeToString :: (Tree Char) -> String
treeToString Empty = []
treeToString (Branch val Empty Empty) = [val]
treeToString (Branch val l r) = [val] ++ ['('] ++ treeToString l ++ [','] ++ treeToString r ++ [')']

stringToTree :: String -> (Tree Char)
stringToTree [] = Empty
stringToTree str = let (x,y,z) = (extractStringParts str) in Branch x (stringToTree y) (stringToTree z)


extractStringParts :: String -> (Char,String,String)
extractStringParts str = let x = (findMiddleStr (drop 2 str) 0 0) in (str!!0, take x (drop 2 str), take ((length str) - (x + 4)) (drop (x + 3) str))

findMiddleStr :: String -> Int -> Int -> Int
findMiddleStr [] _ _ = 0
findMiddleStr (x:xs) braCount pos | x == ',' && braCount == 0 = pos
                                  | x == '(' = findMiddleStr (xs) (braCount + 1) (pos + 1)
                                  | x == ')' = findMiddleStr (xs) (braCount - 1) (pos + 1)
                                  | otherwise = findMiddleStr (xs) braCount (pos + 1)
