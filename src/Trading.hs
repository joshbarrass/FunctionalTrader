module Trading (
    buyPrice
   ,sellPrice
   ,calculateProfit
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
