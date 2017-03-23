module Exercise where

partition :: (a -> Bool) -> [a] -> ([a], [a])
--partition f []     = ([], [])
--partition f (x:xs) = undefined
--partition f a = ([x | x <- a, f x], [x | x <- a, (f x) == False])


partition f [] = ([], [])
partition f (x:xs) = let
                       (left, right) = partition f xs
                      in if f x then (x:left, right) else (left, x:right)
  
-- partition even [1..10] ~> ([2,4,6,8,10],[1,3,5,7,9])
{-
partition f a = (blaa f a, blaa f a)

blaa f (x:xs)
  | f x == True = x:blaa f xs
  | otherwise = blaa f xs
-}

