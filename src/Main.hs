module Main where

import Goods
import Sector

g1 = Good 1 "Wood" 19
g2 = Good 2 "Food" 25

s1 = Sector 1 Wall Wall Wall s2 1 [] [g1, g2]
s2 = Sector 2 Wall Wall s1 s3 1 [g1] []
s3 = Sector 3 Wall Wall s2 Wall 1 [g2] []

main :: IO ()
main = do
  let d1 = distanceIndex g1 s1
  let d2 = distanceIndex g2 s1
  putStrLn $ show d1
  putStrLn $ show d2

