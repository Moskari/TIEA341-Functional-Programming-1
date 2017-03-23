module Exercise where
import Data.Char


uppelis :: String -> String
uppelis str = case str of
    (x:xs) -> toUpper x:xs
    ""     -> ""



fIndex :: (Char->Bool) -> String -> Int -> Maybe Int
fIndex f str i = case str of
                    (x:xs) -> case f x of
                                  True -> Just i
                                  False -> fIndex f xs (i+1)
                    ""     -> Nothing

upperFirst :: String -> String   
upperFirst str = case fIndex (==' ') str 0 of
    Nothing -> uppelis str
    Just first -> let 
                   withStart = take first str
                   withoutStart = drop (first+1) str
                  in (uppelis withStart) ++ " " ++ (upperFirst withoutStart)

