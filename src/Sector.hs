module Sector (
    Sector(..)
   ,distanceIndex
) where

import Goods

data Sector = Wall | Sector { num :: Integer
                            , up :: Maybe Sector
                            , down :: Maybe Sector
                            , left :: Maybe Sector
                            , right :: Maybe Sector
                            , race :: Integer
                            , buys :: [Good]
                            , sells :: [Good]
                            }
instance Eq Sector where
  (==) (Sector a _ _ _ _ _ _ _) (Sector b _ _ _ _ _ _ _)
    | a == b = True
    | otherwise = False
  (==) Wall Wall = True

instance Show Sector where
  show Wall = "Wall"
  show (Sector n u d l r race buys sells) = "Sector(num=" ++ show n ++ ", up=" ++ showNumOnly u ++ ", down=" ++ showNumOnly d ++ ", left=" ++ showNumOnly l ++ ", right=" ++ showNumOnly r ++ ", race=" ++ show race ++  ", buys=" ++ show buys ++ ", sells=" ++ show sells ++ ")"

showNumOnly :: Maybe Sector -> String
showNumOnly Nothing = "Nothing"
showNumOnly (Just Wall) = "Wall"
showNumOnly (Just (Sector n _ _ _ _ _ _ _)) = show n

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
    let distances = map (computeDistanceIndex ((num sec):visited) good) (getAllJust adjacent)
    let justDistances = getAllJust distances
    if (length justDistances) == 0 then Nothing
    else return $ 1 + (foldr1 min justDistances)

distanceIndex :: Good -> Sector -> Maybe Integer
distanceIndex = computeDistanceIndex []

