{-#LANGUAGE ScopedTypeVariables#-}
module Exercise where
import Prelude hiding (foldl,take,drop)

-- Take first n elements from a list
-- >>> take 3 "I am groot"
--     "I a"
take :: Int -> [a] -> [a] 
take n = g . foldr f e
    where 
      g :: (Int -> [a]) -> [a]
      g func = func n
      f :: a -> (Int -> [a]) -> (Int -> [a])
      f x acc =  \m -> case m > 0 of
        True -> x : acc (m-1)
        False -> []
      e = (const [])


-- Remove first n elements from a list
-- >>> drop 3 "m groot"
drop :: Int -> [a] -> [a] 
drop n = g . foldr f e
    where
      g func = func n
      f x acc = \m -> case m > 0 of
        True -> acc (m-1)
        False -> x : acc (m-1)
      e = (const [])
-- Hint: Write types for g, f and e first! 

