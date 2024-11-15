module Main where

import IniParse
import SectorMap ( getSector )
import Sector
import Goods (Good, getGoodByID)
import System.Exit (exitFailure)
import System.Environment
import Data.Foldable (for_)
import Data.Maybe (isNothing, fromJust)

exitOnNothing :: Maybe a -> IO a
exitOnNothing Nothing = exitFailure
exitOnNothing (Just x) = return x

goodType = 9
theGood = getGoodByID goodType

getAllJust :: [Maybe a] -> [a]
getAllJust [] = []
getAllJust [Nothing] = []
getAllJust [Just x] = [x]
getAllJust (x:xs) = (getAllJust [x]) ++ (getAllJust xs)

getIndex :: (Sector -> Good -> Maybe Integer) -> Sector -> IO ()
getIndex index sec = do
  let di = index sec (getGoodByID goodType)
  if isNothing di then return () else do
    putStr "Sector: "
    print $ num sec
    putStr $ "Distance index: "
    print $ fromJust di

getBuyIndex :: Sector -> IO ()
getBuyIndex = getIndex buyDistanceIndex

getSellIndex :: Sector -> IO ()
getSellIndex = getIndex sellDistanceIndex

main :: IO ()
main = do
  (filename:_) <- getArgs
  f <- readFile filename
  ini <- exitOnNothing $ parseIni f
  let creontiSecs = getAllJust $ map (getSector ini) [50..95]
  let goodSellers = filter (`doesSell` theGood) creontiSecs
  let goodBuyers = filter (`doesBuy` theGood) creontiSecs
  putStrLn "== Sellers =="
  for_ goodSellers getSellIndex
  putStrLn "\n== Buyers =="
  for_ goodBuyers getBuyIndex
  
