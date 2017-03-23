module Exercise where 
fib :: Int -> Int
--fib n = undefined

--fib 0 = 1
--fib 1 = 1
fib n = case n > 1 of
  True -> fib (n-1) + fib (n-2)
  False -> 1
