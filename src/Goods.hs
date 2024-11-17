module Goods (
     Good(..)
    ,getGoodByID
    ,allGoods
) where

data Good = Good { id :: Integer
                 , name :: String
                 , price :: Integer
                 , illegal :: Bool
                 }

instance Eq Good where
--  (==) (Good a _ _) (Good a _ _) = True
  (==) (Good a _ _ _) (Good b _ _ _)
    | a == b = True
    | otherwise = False

instance Show Good where
  show = name

getGoodByID :: Integer -> Good
getGoodByID 1 = Good 1 "Wood" 19 False
getGoodByID 2 = Good 2 "Food" 25 False
getGoodByID 3 = Good 3 "Ore" 42 False
getGoodByID 4 = Good 4 "Precious Metals" 62 False
getGoodByID 5 = Good 5 "Slaves" 89 True
getGoodByID 6 = Good 6 "Textiles" 112 False
getGoodByID 7 = Good 7 "Machinery" 126 False
getGoodByID 8 = Good 8 "Circuitry" 141 False
getGoodByID 9 = Good 9 "Weapons" 168 True
getGoodByID 10 = Good 10 "Computers" 196 False
getGoodByID 11 = Good 11 "Luxury Items" 231 False
getGoodByID 12 = Good 12 "Narcotics" 259 True

allGoods :: [Good]
allGoods = map getGoodByID [1..12]
