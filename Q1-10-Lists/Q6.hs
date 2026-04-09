-- Q6, checking if a list in a palindrome
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome myList = myList == reverse myList