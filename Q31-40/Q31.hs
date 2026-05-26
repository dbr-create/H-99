-- Q31, find if an integer is prime

isPrime :: Integer -> Bool
isPrime num | num < 2 = False
            | otherwise = isPrimeHelper 2 num

isPrimeHelper :: Integer -> Integer -> Bool
isPrimeHelper curr num | (curr > (div num 2) + 1) = True
                       | (curr < (div num 2) + 1) && mod num curr == 0 = False
                       | otherwise = isPrimeHelper (curr + 1) num 