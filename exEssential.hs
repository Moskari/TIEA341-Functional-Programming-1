module Tests where
import Test.QuickCheck
import Data.List
import Data.Char

--Tested functions:

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


myConcat :: [[a]] -> [a]
myConcat []    = []
myConcat (x:xs) = x ++ myConcat xs
-- i.e. myConcat [[1,2],[3,4,5],[6]] == [1,2,3,4,5,6]


shout :: String -> String
shout str = case str of
    (x:xs) -> toUpper x:(shout xs)
    ""     -> "!"

nub' :: Eq a => [a] -> [a]
nub' = g . foldr f e
  where
    g = ($ [])
    f x acc = \uniques -> case x `elem` uniques of
      True -> acc (uniques)
      False -> x:acc (x:uniques)
    e = const []

    
-- CASE 1
-- Result of sorting should be the same as sorting the result of sorting
prop_idempotency_mergeSort :: Ord a => [a] -> Bool
prop_idempotency_mergeSort = \xs -> mergeSort (mergeSort xs) == mergeSort xs

-- And it holds:
-- *Tests> quickCheck prop_idempotency_mergeSort
-- +++ OK, passed 100 tests.


-- CASE 2
-- Size of the list should not change after sorting
prop_size_mergeSort :: Ord a => [a] -> Bool
prop_size_mergeSort = \xs -> length (mergeSort xs) == length xs

-- And it holds:
-- *Tests> quickCheck prop_size_mergeSort
-- +++ OK, passed 100 tests.


-- CASE 3
-- Compare mergeSort's results to haskell's reference solution
prop_ref_solution :: Ord a => [a] -> Bool
prop_ref_solution xs = sort xs == mergeSort xs

-- And it holds:
-- *Tests> quickCheck prop_ref_solution
-- +++ OK, passed 100 tests.


-- CASE 4
-- Check that mergeSort sorts regardless of the order of the list
prop_reverse_mergeSort :: Ord a => [a] -> Bool
prop_reverse_mergeSort xs = mergeSort (reverse xs) == mergeSort xs

-- And it holds:
-- *Tests> quickCheck prop_reverse_mergeSort
-- +++ OK, passed 100 tests.

-- CASE 5
-- Check that the number of elements `a` don't change in the structure
prop_size_myConcat :: [[a]] -> Bool
prop_size_myConcat x = length (myConcat x) == (sum $ map (\a -> length a) x)

-- And it holds:
-- *Tests> quickCheck prop_size_myConcat
-- +++ OK, passed 100 tests.


-- CASE 6
-- Check that order of elements stay the same after applying myConcat
prop_order_myConcat :: Eq a => [[a]] -> Bool
prop_order_myConcat x = (myConcat x) == (foldr (++) [] x)

-- And it holds:
-- *Tests> quickCheck prop_order_myConcat
-- +++ OK, passed 100 tests.

-- CASE 7
-- Check that combined size of arrays in merge doesn't change
prop_size_merge :: Ord a => [a] -> [a] -> Bool
prop_size_merge xs ys = (length xs + length ys) == (length $ merge xs ys)

-- And it holds
-- *Tests> quickCheck prop_size_merge
-- +++ OK, passed 100 tests.


-- CASE 8
-- Check that shouting keeps the casing uppercase and adds a "!"
prop_shout :: String -> Bool
prop_shout x = shout x ++ "!" == (shout $ shout x)

-- And it holds
-- *Tests> quickCheck prop_shout
-- +++ OK, passed 100 tests.

-- CASE 9
-- Check that shouting adds string length by 1
prop_size_shout :: String -> Bool
prop_size_shout x = length (shout x) == (length x + 1)

-- And it holds
-- *Tests> quickCheck prop_size_shout
-- +++ OK, passed 100 tests.

-- CASE 10
-- Check that nub keeps list size the same or shorter
prop_nub :: Eq a => [a] -> Bool
prop_nub x = length (nub' x) <= (length x)

-- CASE 11
-- Check that nub doesn't add/change any new values which weren't in the original input list
prop_orig_nub :: Eq a => [a] -> Bool
prop_orig_nub x = (length $ filter (\ a -> not $ a `elem` x) (nub' x)) == 0

-- And it holds
-- *Tests> quickCheck prop_orig_nub
-- +++ OK, passed 100 tests.

-- CASE 12
-- Check that mergeSort's resulting list has items in increasing order
prop_increase_mergeSort :: Ord a => [a] -> Bool
prop_increase_mergeSort x = let
                             isLarger :: Ord a => [a] -> [Bool]
                             isLarger (x0:x1:xs) = (x0 <= x1):isLarger xs
                             isLarger _ = [True]
                            in all id (isLarger (mergeSort x))

-- And it holds
-- *Tests> quickCheck prop_increase_mergeSort
-- +++ OK, passed 100 tests.         

