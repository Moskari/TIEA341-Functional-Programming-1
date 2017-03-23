

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f z (x : xs) = let
                        (y : ys) = scanr' f z xs
                      in f x y : (y : ys)
scanr' _ z _ = [z]

scanl' :: (a -> b -> a) -> a -> [b] -> [a]
scanl' f z (x : xs) = let
                        z' = f z x
                      in z : scanl' f z' xs
scanl' _ z _ = [z]

--foldl f z []     = z
--foldl f z (x:xs) = let z' = z `f` x 
--                   in foldl f z' xs


--foldr f z (x : xs) = let
--                       y = foldr f z xs
--                     in f x y

--foldr f z (x : xs) = f x (foldr f z xs)
--foldr _ z _ = z