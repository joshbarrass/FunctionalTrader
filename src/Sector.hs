module Sector (
    Sector(..)
   ,distanceTo
   ,buyDistanceIndex
   ,sellDistanceIndex
   ,doesSell
   ,doesBuy
) where

import Goods
import Control.Monad.State

data Sector = Wall | Sector { num :: Integer
                            , up :: Maybe Sector
                            , down :: Maybe Sector
                            , left :: Maybe Sector
                            , right :: Maybe Sector
                            , warp :: Maybe Sector
                            , race :: Integer
                            , level :: Integer
                            , buys :: [Good]
                            , sells :: [Good]
                            }
instance Eq Sector where
  (==) (Sector a _ _ _ _ _ _ _ _ _) (Sector b _ _ _ _ _ _ _ _ _)
    | a == b = True
    | otherwise = False
  (==) Wall Wall = True

instance Show Sector where
  show Wall = "Wall"
  show (Sector n u d l r w race level buys sells) = "Sector(num=" ++ show n ++ ", up=" ++ showNumOnly u ++ ", down=" ++ showNumOnly d ++ ", left=" ++ showNumOnly l ++ ", right=" ++ showNumOnly r ++ ", warp=" ++ showNumOnly w ++ ", race=" ++ show race ++ ", level=" ++ show level ++ ", buys=" ++ show buys ++ ", sells=" ++ show sells ++ ")"

isWall :: Sector -> Bool
isWall Wall = True
isWall _ = False

data Search = Search { visited :: [Sector], toVisit :: [Sector], distances :: [Integer]} deriving (Show)
type SearchState = State Search

showNumOnly :: Maybe Sector -> String
showNumOnly Nothing = "Nothing"
showNumOnly (Just Wall) = "Wall"
showNumOnly (Just (Sector n _ _ _ _ _ _ _ _ _)) = show n

getAllJust :: [Maybe a] -> [a]
getAllJust [] = []
getAllJust [Nothing] = []
getAllJust [Just x] = [x]
getAllJust (x:xs) = (getAllJust [x]) ++ (getAllJust xs)

getDistanceTo :: (Sector -> Bool) -> SearchState (Maybe Integer)
getDistanceTo testFunc = do
  -- unpack the state and remove the current sector
  search <- get
  let vis = visited search
  let sec:toVis = toVisit search
  let dist:dists = distances search
  put (Search vis toVis dists)

  -- test default cases
  --  if it's a wall or we've already been here, move on immediately
  --  if it satisfies the test, return the distance
  if isWall sec || (sec `elem` vis) then getDistanceTo testFunc
  else if testFunc sec then return $ Just dist

  -- add adjacent sectors to the search and move on
  else do
    let adjacent = filter (`notElem` vis) $ filter (not . isWall) $ getAllJust (map ($ sec) [up, down, left, right, warp])
    let newDists = [dist + 1 | _ <- adjacent]
    put $ Search (sec:vis) (toVis ++ adjacent) (dists ++ newDists)
    getDistanceTo testFunc

newSearchState :: Sector -> Search
newSearchState start = Search [] [start] [0]

distanceTo :: Sector -> (Sector -> Bool) -> Maybe Integer
distanceTo sec testFunc = evalState (getDistanceTo testFunc) (newSearchState sec)

-- distance index for a sector selling to the player, based on the
-- distance to the nearest sector which buys the good
sellDistanceIndex :: Sector -> Good -> Maybe Integer
sellDistanceIndex sec good = sec `distanceTo` (elem good . buys)

-- distance index for a sector buying from the player, based on the
-- distance to the nearest sector which sells the good
buyDistanceIndex :: Sector -> Good -> Maybe Integer
buyDistanceIndex sec good = sec `distanceTo` (elem good . sells)

doesSell :: Sector -> Good -> Bool
doesSell sec good = good `elem` sells sec

doesBuy :: Sector -> Good -> Bool
doesBuy sec good = good `elem` buys sec
