-- a.
unfoldr :: (b -> Maybe (a,b)) -> b -> [a]
unfoldr f z = let
                z' = f z
              in case z' of
                Just (a, b)  -> a:unfoldr f b
                Nothing      -> []

-- unfoldr (\b -> if b == 0 then Nothing else Just (b, b-1)) 10


-- b.

naturalNumbers = unfoldr (\b -> Just (b, b+1)) 1

-- c.

zip' :: ([a],[b]) -> [(a,b)]
zip' t = let
          -- f :: b -> Maybe (c,b)
          f :: ([a1], [a2]) -> Maybe ((a1, a2),([a1], [a2]))
          f (x:xs, y:ys) = Just ((x, y), (xs, ys))
          f _ = Nothing
        in unfoldr f t

-- take 12 $ zip' ([1..], [2..])