module Exercise where

delete :: Eq a => a -> [a] -> [a]
delete r [] = []
delete r (x:xs)
  | r == x = xs
  | otherwise = x:delete r xs

takeEvens :: [Int] -> [Int]
takeEvens xs = [x | x <- xs, even x]

(+++) :: [a] -> [a] -> [a]
[]     +++ bs = bs
(a:as) +++ bs = a:(as +++ bs)

myConcat :: [[a]] -> [a]
myConcat []    = []
myConcat (x:xs) = x ++ myConcat xs
