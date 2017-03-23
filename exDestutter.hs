module Exercise where

destutter :: Eq a => [a] -> [a]
destutter [] = []
destutter (x0:x1:xs) = case x0 == x1 of
  True -> let y:ys = destutter (x1:xs) in y:ys
  False -> x0:destutter (x1:xs)
destutter (x0:xs) = [x0]