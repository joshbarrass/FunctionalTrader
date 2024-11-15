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

isWall :: Sector -> Bool
isWall Wall = True
isWall _ = False

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
computeDistanceIndex (SearchState _ [] _) _ = Nothing
computeDistanceIndex (SearchState visited (sec:toVisit) (dist:distances)) good
  | isWall sec = computeDistanceIndex (SearchState visited toVisit distances) good
  | n `elem` visited = computeDistanceIndex (SearchState visited toVisit distances) good
  | good `elem` (buys sec) = Just dist
  | otherwise = do
    let adjacent = filter (\s -> num s `notElem` visited) $ filter (not . isWall) $ getAllJust [up sec, down sec, left sec, right sec, warp sec]
    let newDists = [dist + 1 | _ <- adjacent]
    computeDistanceIndex (SearchState (n:visited) (toVisit ++ adjacent) (distances ++ newDists)) good
  where n = num sec

distanceIndex :: Sector -> Good -> Maybe Integer
distanceIndex sec = computeDistanceIndex (SearchState [] [sec] [0])

