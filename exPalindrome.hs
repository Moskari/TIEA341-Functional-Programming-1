module Exercise where
import Data.List

isPalindrome :: Eq a => [a] -> Bool
isPalindrome a = a == (reverse a)
