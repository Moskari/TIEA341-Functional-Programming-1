module Examples where

data Bit = I | O deriving (Show,Eq)
type Binary = [Bit]

-- Write 5 examples of how incrementing should work
-- in the following list. First element of each pair
-- is the input and the second one is the first one
-- incremented by one.
incrementedByOne :: [(Binary,Binary)]
incrementedByOne = [([I,O],[O,I]), ([O],[I]), ([O,I],[I,I]), ([I,I],[O,O,I]), ([O,O,I],[I,O,I])]


increment :: Binary -> Binary
increment [] = [I]
increment (x:xs) = case x of
  O -> I:xs
  I -> O:(increment xs)
{-
fromBinary :: Binary -> Int
fromBinary [] = 0
fromBinary (x:xs) = let 
  helper [] _ = 0
  helper (y:ys) i = case y of
    O -> 0 + helper ys (i+1)
    I -> 2^i + helper ys (i+1)
  in helper (x:xs) 0
-}
fromBinary :: Binary -> Int
fromBinary [] = 0
-- fromBinary (x:xs) =  ((if x == O then 0 else 1) + 2*  fromBinary xs)
fromBinary (O:xs) =  2 * fromBinary xs
fromBinary (I:xs) =  2 * fromBinary xs + 1

toBinary :: Int -> Binary
toBinary 0 = []
toBinary a = let 
    r = a `mod` 2
    n = a `div` 2
  in case r of
    1 -> I:toBinary (n)
    0 -> O:toBinary (n)

-- Here is something that is just needed by the web exercise system. Please
-- leave it alone, ok?
isOne I=True;isOne O=False;bit True=I;bit False=O           

