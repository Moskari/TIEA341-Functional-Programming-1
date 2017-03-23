module Exercise where
import Data.List hiding (sort)

deal :: [a] -> ([a],[a])
deal [] = ([], [])
deal a = let
          l = length a
          middle = floor (fromIntegral l/2)
         in (take middle a, drop middle a)

merge :: Ord a => [a] -> [a] -> [a]
merge [] [] = []
merge [] (y:ys) = (y:ys)
merge (y:ys) [] = (y:ys)
merge (x:xs) (y:ys) = case x < y of
 True  -> x:merge xs (y:ys)
 False -> y:merge (x:xs) ys

mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [a] = [a]
--mergeSort a = merge (mergeSort (fst (deal a))) (mergeSort (snd (deal a)))
mergeSort a = merge (mergeSort . fst $ deal a) (mergeSort . snd . deal $ a)

