module Exercise where

pythagoreanTriples :: Integer -> [(Integer,Integer,Integer)]
pythagoreanTriples x = [(a,b,c) | a<-[1..x], b<-[1..x], c<-[1..x],  a^2 + b^2 == c^2]
