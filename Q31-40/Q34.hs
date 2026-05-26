-- Q34, calulate the phi-totient of a given value m

totient :: Int -> Int
totient num = totientHelper 1 num 0

totientHelper :: Int -> Int -> Int -> Int
totientHelper num1 num2 counter | num1 == num2 = counter
                                | myGcd num1 num2 == 1 = totientHelper (num1 + 1) num2 (counter + 1)
                                | otherwise = totientHelper (num1 + 1) num2 counter

myGcd :: Int -> Int -> Int
myGcd 0 _ = 0
myGcd _ 0 = 0
myGcd num1 num2 | (negateHelper num1) >= (negateHelper num2) = gcdHelper (negateHelper num1) (negateHelper num2)
                | otherwise = gcdHelper (negateHelper num2) (negateHelper num1)


negateHelper :: Int -> Int
negateHelper num | num < 0 = (-num)
                 | num > 0 = num

gcdHelper :: Int -> Int -> Int
gcdHelper num1 num2 | mod num1 num2 == 0 = div num1 (div num1 num2)
                    | otherwise = gcdHelper (num1 - (mod num1 num2)) (mod num1 num2)