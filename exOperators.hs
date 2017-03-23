module Main where

--sequence :: [IO a] -> IO [a]
--sequence [] = pure []
--sequence (op:ops) = do
--    x <- op
--    xs <- sequence ops
--    return (x:xs)

sequence' :: [IO a] -> IO [a]
sequence' [] = pure []
sequence' (op:ops) = op >>= \x -> (sequence' ops >>= \y -> pure (x:y))

main :: IO ()
main = do
  putStrLn "hello world"
