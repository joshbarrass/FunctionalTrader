module Sector (
    Sector(..)
   ,distanceTo
   ,buyDistanceIndex
   ,sellDistanceIndex
   ,doesSell
   ,doesBuy
) where

import Goods
import Data.List (insertBy)
import Data.Maybe (isJust, fromJust)
import Data.Bifunctor (first)
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

data Search = Search { visited :: [Sector], toVisit :: [(Sector, Integer)]} deriving (Show)
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
  Search vis toVisit <- get
  if null toVisit then return Nothing else do
    let ((sec, dist):toVis) = toVisit
    put (Search vis toVis)

    -- test default cases
    --  if it's a wall or we've already been here, move on immediately
    --  if it satisfies the test, return the distance
    if isWall sec || (sec `elem` vis) then getDistanceTo testFunc
    else if testFunc sec then return $ Just dist

    -- add adjacent sectors to the search and move on
    else do
      let adjacent = map ($ sec) [up, down, left, right, warp]
      let secDistPairs = zip adjacent ([dist + 1 | _ <- tail adjacent] ++ [dist + 5])
      let validPairs = filter ((`notElem` vis) . fst ) $
                       filter (not . isWall . fst) $
                       map (first fromJust) $
                       filter (isJust . fst) secDistPairs
      put $ Search (sec:vis) $ insertAllBy distCmp validPairs toVis
      getDistanceTo testFunc
  where distCmp :: ((Sector, Integer) -> (Sector, Integer) -> Ordering)
        distCmp (_, d1) (_, d2) = compare d1 d2

newSearchState :: Sector -> Search
newSearchState start = Search [] [(start, 0)]

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

insertAllBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
insertAllBy _ [] ys = ys
insertAllBy f [x] ys = insertBy f x ys
insertAllBy f (x:xs) ys = insertAllBy f xs $ insertBy f x ys
