module Lib
    ( 
    ) where
import Data.Char
import Data.List
    
-- a.
shout :: String -> String
shout str = case str of
    (x:xs) -> toUpper x:(shout xs)
    ""     -> "!"

-- b.
shoutWords :: String -> String
shoutWords str = case findIndex (==' ') str of
    Nothing -> shout str
    Just first -> let 
                   withStart = take first str
                   withoutStart = drop (first+1) str
                  in (shout withStart) ++ " " ++ (shoutWords withoutStart)
                  
-- c.
shoutLines :: String -> String
shoutLines str = case findIndex (=='\n') str of
    Nothing -> shout str
    Just first -> let 
                   withStart = take first str
                   withoutStart = drop (first+1) str
                  in (shout withStart) ++ "\n" ++ (shoutLines withoutStart)
                  