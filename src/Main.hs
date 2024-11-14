module Main where

import Goods
import Sector

g1 = Good 1 "Wood" 19
g2 = Good 2 "Food" 25

s1 = Sector 1 (Just Wall) (Just Wall) (Just Wall) (Just s2) 1 [] [g1, g2]
s2 = Sector 2 (Just Wall) (Just Wall) (Just s1) (Just s3) 1 [g1] []
s3 = Sector 3 (Just Wall) (Just Wall) (Just s2) (Just Wall) 1 [g2] []

main :: IO ()
main = do
  let d1 = distanceIndex g1 s1
  let d2 = distanceIndex g2 s1
  print d1
  print d2

