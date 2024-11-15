module Goods (
     Good(..)
    ,getGoodByID
    ,allGoods
) where

data Good = Good { id :: Integer
                 , name :: String
                 , price :: Integer
                 }

instance Eq Good where
--  (==) (Good a _ _) (Good a _ _) = True
  (==) (Good a _ _) (Good b _ _)
    | a == b = True
    | otherwise = False

instance Show Good where
  show = name

getGoodByID :: Integer -> Good
getGoodByID 1 = Good 1 "Wood" 19
getGoodByID 2 = Good 2 "Food" 25
getGoodByID 3 = Good 3 "Ore" 42
getGoodByID 4 = Good 4 "Precious Metals" 62
getGoodByID 5 = Good 5 "Slaves" 89
getGoodByID 6 = Good 6 "Textiles" 112
getGoodByID 7 = Good 7 "Machinery" 126
getGoodByID 8 = Good 8 "Circuitry" 141
getGoodByID 9 = Good 9 "Weapons" 168
getGoodByID 10 = Good 10 "Computers" 196
getGoodByID 11 = Good 11 "Luxury Items" 231
getGoodByID 12 = Good 12 "Narcotics" 259

allGoods :: [Good]
allGoods = map getGoodByID [1..12]
