module Main where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.List

class JSON a where
    toJSONString :: a -> String


instance JSON Int where
    toJSONString x = show x

instance JSON Text where
    toJSONString x = "'" ++ Text.unpack x ++ "'"

instance JSON Char where
    toJSONString x = show x
    
instance (JSON a) => JSON [a] where
    toJSONString xs = "["++(intercalate "," $ map toJSONString xs) ++ "]"


instance (JSON a, JSON b) => JSON (a, b) where
    toJSONString (x, y) = "{'fst':" ++ toJSONString x ++ ", 'snd':" ++ toJSONString y ++ "}"


main :: IO ()
main = do
  putStrLn (toJSONString ([1,2,3::Int],[(Text.pack "cat",(6::Int,'a')),(Text.pack "dog",(7::Int,'x'))]))
