module Sector (
    Sector(..)
   ,distanceIndex
) where

import Goods
import Debug.Trace

data Sector = Wall | Sector { num :: Integer
                            , up :: Maybe Sector
                            , down :: Maybe Sector
                            , left :: Maybe Sector
                            , right :: Maybe Sector
                            , warp :: Maybe Sector
                            , race :: Integer
                            , buys :: [Good]
                            , sells :: [Good]
                            }
instance Eq Sector where
  (==) (Sector a _ _ _ _ _ _ _ _) (Sector b _ _ _ _ _ _ _ _)
    | a == b = True
    | otherwise = False
  (==) Wall Wall = True

instance Show Sector where
  show Wall = "Wall"
  show (Sector n u d l r w race buys sells) = "Sector(num=" ++ show n ++ ", up=" ++ showNumOnly u ++ ", down=" ++ showNumOnly d ++ ", left=" ++ showNumOnly l ++ ", right=" ++ showNumOnly r ++ ", warp=" ++ showNumOnly w ++ ", race=" ++ show race ++  ", buys=" ++ show buys ++ ", sells=" ++ show sells ++ ")"

data SearchState = SearchState { visited :: [Integer], toVisit :: [Sector], distances :: [Integer]} deriving (Show)

showNumOnly :: Maybe Sector -> String
showNumOnly Nothing = "Nothing"
showNumOnly (Just Wall) = "Wall"
showNumOnly (Just (Sector n _ _ _ _ _ _ _ _)) = show n

getAllJust :: [Maybe a] -> [a]
getAllJust [] = []
getAllJust [Nothing] = []
getAllJust [Just x] = [x]
getAllJust (x:xs) = (getAllJust [x]) ++ (getAllJust xs) 

computeDistanceIndex :: SearchState -> Good -> Maybe Integer
computeDistanceIndex (SearchState _ (Wall:_) _) _ = Nothing
computeDistanceIndex (SearchState visited (sec:toVisit) (dist:distances)) good
  | n `elem` visited = Nothing
  | good `elem` (buys sec) = Just dist
  | otherwise = do
    let adjacent = getAllJust [up sec, down sec, left sec, right sec, warp sec]
    let newDists = [dist + 1 | _ <- adjacent]
    if length adjacent == 0 then Nothing else
      computeDistanceIndex (SearchState (n:visited) (toVisit ++ adjacent) (distances ++ newDists)) good
  where n = num sec

distanceIndex :: Sector -> Good -> Maybe Integer
distanceIndex sec = computeDistanceIndex (SearchState [] [sec] [0])

