-- Q28b, sort by frequency of length



lfSort :: [[a]] -> [[a]]
lfSort [] = []
lfSort values = concat (lsSort (groupBySize (lsSort values)))

groupBySize :: [[a]] -> [[[a]]]
groupBySize [] = []
groupBySize values = let currentLength = (length (values!!0)) in ( let sameSize = [ val | val <- values, (length val) == currentLength] in ([sameSize]++ groupBySize (drop ((length sameSize)) values)))

lsSort :: [[a]] -> [[a]]
lsSort [] = []
lsSort values = let minPos = (findMinLengthPos 0 0 (length (values!!0)) values) in
                ([values!!minPos] ++ (lsSort ((take minPos values) ++ (drop (minPos + 1) values))))

findMinLengthPos :: Int -> Int -> Int -> [[a]] -> Int
findMinLengthPos minPos _ _ [] = minPos
findMinLengthPos minPos currPos subLength (x:xs) | (length x) < subLength = findMinLengthPos currPos (currPos + 1) (length x) (xs)
                                                 | otherwise = findMinLengthPos minPos (currPos + 1) subLength (xs)