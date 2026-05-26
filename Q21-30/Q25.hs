-- Q25, generate a random permutation of a list

import System.Random

rnd_permu :: [a] -> IO [a]
rnd_permu [] = return []
rnd_permu myList = do
  indx <- randomRIO (0, (length myList) - 1)
  rest <- rnd_permu ((take (indx) myList) ++ (drop (indx + 1) myList))
  return ((myList !! indx) : rest)
