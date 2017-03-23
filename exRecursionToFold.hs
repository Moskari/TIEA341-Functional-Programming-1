-- {-# LANGUAGE BangPatterns #-}
import Data.List

--foldl' f z []     = z
--foldl' f (!z) (x:xs) = foldl f (f z x) xs


length' :: [a] -> Int
length' a = foldr (\x -> (1+)) 0 a

concat' :: [[a]] -> [a]
concat' a = foldr (++) [] a

{-
data AlreadyErased = Erased | NotErased
remove' :: Eq a => a -> [a] -> [a]
remove' r = fst . foldr op ([], NotErased)
  where
    op first (xs, Erased) = (first:xs, Erased)
    op first (xs, NotErased)
      | first == r = 
-}

data AlreadyErased = Erased | NotErased
remove' :: Eq a => a -> [a] -> [a]
remove' x = post . foldr op empty
  where
   post     = fst
   op first (erasedTail,Erased)    = (first:erasedTail,Erased)
   op first (erasedTail,NotErased)
             | first == x = (erasedTail,Erased)
             | otherwise  = (first:erasedTail,NotErased)
   empty    = ([],NotErased)

find' :: (a->Bool) -> [a] -> Maybe a
find' func = g . foldr f e
  where
    g = id
    f x rest
      | func x = Just x
      | otherwise = rest
    e = Nothing

filter' :: (a->Bool) -> [a] -> [a]
filter' func = g . foldr f e
  where
    g = id
    f x rest
      | func x = x:rest
      | otherwise = rest
    e = []

take' :: Int -> [a] -> [a]
take' n = g . foldr f e
  where 
    g :: (Int -> [a]) -> [a]
    g func = func n
    f :: a -> (Int -> [a]) -> (Int -> [a])
    f x acc =  \m -> case m > 0 of
      True -> x : acc (m-1)
      False -> []
    e = (const [])

nub' :: Eq a => [a] -> [a]
nub' = g . foldr f e
  where
    g = ($ [])
    f x acc = \uniques -> case x `elem` uniques of
      True -> acc (uniques)
      False -> x:acc (x:uniques)
    e = const []
