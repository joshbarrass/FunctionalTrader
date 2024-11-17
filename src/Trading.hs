module Trading (
    buyPrice
   ,sellPrice
   ,calculateProfit
   ,calculateProfitPerTurn
   ,calculateTwoWayProfit
   ,calculateTwoWayProfitPerTurn
               ) where

-- import Math
import Sector
import Goods ( Good(price, illegal) )
import Data.Maybe
import qualified Data.Map as Map

type Relations = Map.Map Integer Integer

getRelation :: Relations -> Integer -> Integer
getRelation rels race = fromMaybe 0 rel
  where rel = Map.lookup race rels

getRelFactor :: Integer -> Double
getRelFactor rel = if factor > 1 then 1 else factor
  where factor = fromIntegral rel / 1000.0

-- the lowest price a sector will accept for a player buying this good
-- from them
buyPrice :: Relations -> Sector -> Good -> Maybe Integer
buyPrice rels sec good = do
  -- ports won't trade with us if our rel is -300 or less
  if rel <= (-300) then Nothing else do
    distanceIndex <- sellDistanceIndex sec good
    return $ ceiling $ 0.03 * (fromIntegral $ price good) * ((fromIntegral distanceIndex)**1.3) * (2 - 1) * (3 - 2*relFactor)
  where rel = getRelation rels (race sec)
        relFactor = getRelFactor rel

-- the highest price a sector will accept for a player selling this
-- good to them
sellPrice :: Relations -> Sector -> Good -> Maybe Integer
sellPrice rels sec good = do
  -- ports won't trade with us if our rel is -300 or less
  if rel <= (-300) then Nothing else do
    distanceIndex <- buyDistanceIndex sec good
    return $ floor $ 0.088 * (fromIntegral $ price good) * ((fromIntegral distanceIndex)**1.3) * (1 + 1) * (1.2 + 1.8*relFactor)
  where rel = getRelation rels (race sec)
        relFactor = getRelFactor rel

calculateProfit :: Relations -> Sector -> Sector -> Good -> Maybe Integer
calculateProfit rels buyFrom sellTo good
  | not (buyFrom `doesSell` good) = Nothing
  | not (sellTo `doesBuy` good) = Nothing
  | otherwise = do
    buyFor <- buyPrice rels buyFrom good
    sellFor <- sellPrice rels sellTo good
    return $ sellFor - buyFor

calculateProfitPerTurn :: (Fractional a) => Relations -> Sector -> Sector -> Good -> Maybe a
calculateProfitPerTurn rels buyFrom sellTo good = do
  profit <- calculateProfit rels buyFrom sellTo good
  distance <- buyFrom `distanceTo` ((num sellTo ==) . num)
  return $ fromIntegral profit / fromIntegral (distance + 2)

calculateTwoWayProfit :: Relations -> Sector -> Sector -> Good -> Good -> Maybe Integer
calculateTwoWayProfit rels port1 port2 sellAt2 sellAt1 = do
  way1 <- calculateProfit rels port1 port2 sellAt2
  way2 <- calculateProfit rels port2 port1 sellAt1
  return $ way1 + way2

calculateTwoWayProfitPerTurn :: (Fractional a) => Relations -> Sector -> Sector -> Good -> Good -> Maybe a
calculateTwoWayProfitPerTurn rels port1 port2 sellAt2 sellAt1 = do
  profit <- calculateTwoWayProfit rels port1 port2 sellAt2 sellAt1
  distance <- port1 `distanceTo` ((num port2 ==) . num)
  return $ fromIntegral profit / fromIntegral (2*distance + 4)

-- per good
calculateFine :: Good -> Sector -> Integer
calculateFine good sector = price good * level sector

calculateInspectionChance :: Sector -> Double
calculateInspectionChance sector = baseChance - reductionPerIllegal * numIllegal
  where numIllegal = fromIntegral $ length (filter illegal (sells sector)) + length (filter illegal (buys sector))
        baseChance = 0.15
        reductionPerIllegal = 0.04
