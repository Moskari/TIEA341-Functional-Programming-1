module Exercise where
import Data.List hiding (foldl)

-- Extract the first element from a list
fhead :: [a] -> Maybe a 
fhead = g . foldr f e
    where 
      g = id
      f x m = Just x
      e = Nothing

-- Extract the last element from a list
flast :: [a] -> Maybe a 
flast = g . foldl' f e
    where 
      g = id
      f x m = Just m
      e = Nothing

-- Extract all but the first element of a list
ftail :: [a] -> Maybe [a]
ftail = g . foldr f e
    where 
      g func = func True
      f x acc = \m -> case m of
        True -> acc False
        False -> case acc False of
          ~(Just rest) -> Just (x:rest)
      e = (const Nothing)

-- Extract all but the last element from the list
finit :: [a] -> Maybe [a]
finit = g . foldr f e
    where 
      g func = func True
      f x acc = \m -> case m of
        True -> acc False
        False -> case acc False of
          ~(Just rest) -> Just (x:rest)
      e = (const Nothing)
