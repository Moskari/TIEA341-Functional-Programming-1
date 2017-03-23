module Main where




type Queen = (Int,Int)
type Setup = [Queen]



isgood :: (Int, Int) -> (Int, Int) -> Bool
isgood (a1, a2) (b1, b2)
   | (a1,a2) == (b1,b2) = False  -- Queen doesn't threaten herself
   | a2 == b2           = False  -- On the same row
   | a1 == b1           = False  -- On the same column
   | (a1-a2) == (b1-b2) = False  -- diagonal
   | (a1+a2) == (b1+b2) = False  -- diagonal
   | otherwise   = True
           
merge :: [[Int]] -> [[Int]] -> [[Int]]
merge qs1 qs2 = [(map snd (q1++q2)) 
                | q1 <- map (zip [1..]) qs1
                , q2 <- map (\ xs -> zip [(1 + length xs)..] xs) qs2
                , and [isgood uh oh | uh <- q1, oh <- q2]
                ]
{- [[1,2,3,4], [2,1,4,5]] -}

queens4 :: Int -> Int -> [Setup]
queens4 h w
  | w == 1    = map (zip [1..]) [[x] | x <- [1..h]]
  | otherwise = map (zip [1..]) $ merge ((map . map) snd $ queens4 h (w `quot` 2)) ((map . map) snd $ queens4 h (w `quot` 2))

main :: IO ()
main = do
  putStrLn "hello world"
