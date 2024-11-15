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
import Goods ( Good(price) )

-- the lowest price a sector will accept for a player buying this good
-- from them
buyPrice :: Sector -> Good -> Maybe Integer
buyPrice sec good = do
  distanceIndex <- sellDistanceIndex sec good
  return $ ceiling $ 0.03 * (fromIntegral $ price good) * ((fromIntegral distanceIndex)**1.3) * (2 - 1) * (3)

-- the highest price a sector will accept for a player selling this
-- good to them
sellPrice :: Sector -> Good -> Maybe Integer
sellPrice sec good = do
  distanceIndex <- buyDistanceIndex sec good
  return $ floor $ 0.088 * (fromIntegral $ price good) * ((fromIntegral distanceIndex)**1.3) * (1 + 1) * (1.2)

calculateProfit :: Sector -> Sector -> Good -> Maybe Integer
calculateProfit buyFrom sellTo good
  | not (buyFrom `doesSell` good) = Nothing
  | not (sellTo `doesBuy` good) = Nothing
  | otherwise = do
    buyFor <- buyPrice buyFrom good
    sellFor <- sellPrice sellTo good
    return $ sellFor - buyFor

calculateProfitPerTurn :: (Fractional a) => Sector -> Sector -> Good -> Maybe a
calculateProfitPerTurn buyFrom sellTo good = do
  profit <- calculateProfit buyFrom sellTo good
  distance <- buyFrom `distanceTo` ((num sellTo ==) . num)
  return $ fromIntegral profit / fromIntegral (distance + 2)

calculateTwoWayProfit :: Sector -> Sector -> Good -> Good -> Maybe Integer
calculateTwoWayProfit port1 port2 sellAt2 sellAt1 = do
  way1 <- calculateProfit port1 port2 sellAt2
  way2 <- calculateProfit port2 port1 sellAt1
  return $ way1 + way2

calculateTwoWayProfitPerTurn :: (Fractional a) => Sector -> Sector -> Good -> Good -> Maybe a
calculateTwoWayProfitPerTurn port1 port2 sellAt2 sellAt1 = do
  profit <- calculateTwoWayProfit port1 port2 sellAt2 sellAt1
  distance <- port1 `distanceTo` ((num port2 ==) . num)
  return $ fromIntegral profit / fromIntegral (2*distance + 4)
