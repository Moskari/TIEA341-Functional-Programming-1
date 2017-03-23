module Exercise where
import Data.List
import Data.Word                    -- .. different lengths of integers
import Data.Char (ord,chr)          -- .. conversion between char and int


encode :: String -> [(Char,Word8)]
encode (x0:x1:xs) = case x0 == x1 of
  True -> let
           (y, n):ys = encode (x1:xs)
          in (y, n+1):ys
  False -> (x0, 1):encode (x1:xs)
encode (x0:xs) = [(x0,1)]
encode [] = []

-- > encode "aaaaabbbc"
-- [('a',5),('b',3),('c',1)]


decode :: [(Char,Word8)] -> String
decode [] = ""
decode ((x,n) : xs) = (replicate (fromIntegral n) x) ++ (decode xs)
