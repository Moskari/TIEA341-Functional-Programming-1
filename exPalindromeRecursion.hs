module Exercise where         
import qualified Data.Array as A
import Data.Array (Array,(!))

exampleArray :: Array Int Char
exampleArray   = A.listArray (0,4) "abiba"
fromString str = A.listArray (0,length str - 1) str

isPalindrome :: Array Int Char -> Bool
--isPalindrome arr  = arr == a where a = fromString [arr ! i | i <- [0..5]]

isPalindrome arr = let
    (f, l) = A.bounds arr
    helper arr i = case i > (l `div` 2) of
      True  -> True
      False -> case (arr ! i) == (arr ! (l-i)) of
        True -> helper arr (i+1)
        False -> False
  in helper arr 0