module Sector (
    Sector(..)
   ,distanceIndex
) where

import Goods

data Sector = Wall | Sector { num :: Integer
                            , up :: Sector
                            , down :: Sector
                            , left :: Sector
                            , right :: Sector
                            , race :: Integer
                            , buys :: [Good]
                            , sells :: [Good]
                            } deriving (Show)

getAllJust :: [Maybe a] -> [a]
getAllJust [] = []
getAllJust [Nothing] = []
getAllJust [Just x] = [x]
getAllJust (x:xs) = (getAllJust [x]) ++ (getAllJust xs) 

computeDistanceIndex :: [Integer] -> Good -> Sector -> Maybe Integer
computeDistanceIndex _ _ Wall = Nothing
computeDistanceIndex visited good sec
  | (num sec) `elem` visited = Nothing
  | good `elem` (buys sec) = Just 0
  | otherwise = do
    let adjacent = [up sec, down sec, left sec, right sec]
    let distances = map (computeDistanceIndex ((num sec):visited) good) adjacent
    let justDistances = getAllJust distances
    if (length justDistances) == 0 then Nothing
    else return $ 1 + (foldr1 min justDistances)

distanceIndex :: Good -> Sector -> Maybe Integer
distanceIndex = computeDistanceIndex []
