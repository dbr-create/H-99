-- Q28a, sort a 2D array by length of sublists

lsSort :: [[a]] -> [[a]]
lsSort [] = []
lsSort values = let minPos = (findMinLengthPos 0 0 (length (values!!0)) values) in
                ([values!!minPos] ++ (lsSort ((take minPos values) ++ (drop (minPos + 1) values))))

findMinLengthPos :: Int -> Int -> Int -> [[a]] -> Int
findMinLengthPos minPos _ _ [] = minPos
findMinLengthPos minPos currPos subLength (x:xs) | (length x) < subLength = findMinLengthPos currPos (currPos + 1) (length x) (xs)
                                                 | otherwise = findMinLengthPos minPos (currPos + 1) subLength (xs)