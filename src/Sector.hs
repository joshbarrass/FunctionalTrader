module Sector (
    Sector(..)
   ,getDistanceTo
   ,buyDistanceIndex
   ,sellDistanceIndex
   ,doesSell
   ,doesBuy
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

getDistanceTo :: SearchState -> (Sector -> Bool) -> Maybe Integer
getDistanceTo (SearchState _ [] _) _ = Nothing
getDistanceTo (SearchState visited (sec:toVisit) (dist:distances)) testFunc
  | isWall sec = getDistanceTo (SearchState visited toVisit distances) testFunc
  | n `elem` visited = getDistanceTo (SearchState visited toVisit distances) testFunc
  | testFunc sec = Just dist
  | otherwise = do
    let adjacent = filter (\s -> num s `notElem` visited) $ filter (not . isWall) $ getAllJust [up sec, down sec, left sec, right sec, warp sec]
    let newDists = [dist + 1 | _ <- adjacent]
    getDistanceTo (SearchState (n:visited) (toVisit ++ adjacent) (distances ++ newDists)) testFunc
  where n = num sec

-- distance index for a sector selling to the player, based on the
-- distance to the nearest sector which buys the good
sellDistanceIndex :: Sector -> Good -> Maybe Integer
sellDistanceIndex sec good = getDistanceTo (SearchState [] [sec] [0]) (elem good . buys)

-- distance index for a sector buying from the player, based on the
-- distance to the nearest sector which sells the good
buyDistanceIndex :: Sector -> Good -> Maybe Integer
buyDistanceIndex sec good = getDistanceTo (SearchState [] [sec] [0]) (elem good . sells)

doesSell :: Sector -> Good -> Bool
doesSell sec good = good `elem` sells sec

doesBuy :: Sector -> Good -> Bool
doesBuy sec good = good `elem` buys sec
