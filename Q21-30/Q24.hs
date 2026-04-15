-- Q24, select k random values between 1..H
import System.Random

diff_select :: Integer -> Integer -> IO [Integer]
diff_select 0 high = return []
diff_select amount high = do
  random <- randomRIO (1, high)
  rest <- diff_select (amount - 1) high
  return ([random] ++ rest)
