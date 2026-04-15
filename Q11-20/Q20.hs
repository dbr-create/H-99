-- Q20, removing an element at position k
-- While the use of Maybe Monad was not specified in the question I decided it was best to account for erroneous input

removeAt :: Int -> [a] -> (Maybe a, [a])
removeAt _ [] = (Nothing, [])
removeAt count myList = removeAtHelper count myList []

removeAtHelper :: Int -> [a] -> [a] -> (Maybe a, [a])
removeAtHelper 1 (current : []) outList = (Just current, outList)
removeAtHelper count (current : []) outList = (Nothing, outList ++ [current])
removeAtHelper count (current : rest) outList
  | count < 2 = (Just current, (outList ++ rest))
  | otherwise = removeAtHelper (count - 1) (rest) (outList ++ [current])
