module Goods (
    Good(..),
) where

data Good = Good { id :: Integer
                 , name :: String
                 , price :: Integer
                 } deriving (Show)

instance Eq Good where
--  (==) (Good a _ _) (Good a _ _) = True
  (==) (Good a _ _) (Good b _ _)
    | a == b = True
    | otherwise = False
