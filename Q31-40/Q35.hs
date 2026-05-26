--Q35, find all prime factors of a number

primeFactors :: Int -> [Int]
primeFactors num | num > 1 = primeFactorsHelper 2 num
                 | otherwise = []

primeFactorsHelper :: Int -> Int -> [Int]
primeFactorsHelper currVal num | currVal == num = [currVal]
                               | mod num currVal == 0 = [div num (div num currVal)] ++ primeFactorsHelper currVal (div num currVal)
                               | otherwise = primeFactorsHelper (currVal + 1) num