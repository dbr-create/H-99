-- Q23, take k random values form a list
import System.Random

rnd_select :: [a] -> Integer -> IO [a]
rnd_select myList 0 = return []
rnd_select myList amount = do
  indx <- randomRIO (0, ((length myList) - 1))
  rest <- rnd_select myList (amount - 1)
  return ([myList !! indx] ++ rest)