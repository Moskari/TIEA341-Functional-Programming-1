module Main where

import Data.Time.Clock
import Data.Time.Calendar

data Season = Winter | Spring | Summer | Autumn deriving Show
data Month  = Jan | Feb | Mar | Apr | May | Jun 
            | Jul | Aug | Sep | Oct | Nov | Dec deriving (Show,Eq,Ord)
data Event = MayDay | IndependenceDay | MothersDay 


class Seasonal a where
    season :: a -> Season



instance Seasonal Month where
    season Jan = Winter
    season Feb = Winter
    season Mar = Spring
    season Apr = Spring
    season May = Spring
    season Jun = Summer
    season Jul = Summer
    season Aug = Summer
    season Sep = Autumn
    season Oct = Autumn
    season Nov = Autumn
    season Dec = Winter
    
instance Seasonal Event where
    season MayDay = Spring
    season IndependenceDay = Winter
    season MothersDay = Spring
    
instance Seasonal UTCTime where
   season (UTCTime day b) = let
                              (_, month, _) = toGregorian day
                            in case month of
                              1  -> Winter
                              2  -> Winter
                              3  -> Spring
                              4  -> Spring
                              5  -> Spring
                              6  -> Summer
                              7  -> Summer
                              8  -> Summer
                              9  -> Autumn
                              10  -> Autumn
                              11  -> Autumn
                              12  -> Winter
                              
                              
                              
                              
                              
                              
main :: IO ()
main = do
  putStrLn "hello world"
