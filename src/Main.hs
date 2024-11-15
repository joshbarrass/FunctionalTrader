module Main where

import IniParse
import SectorMap ( getSector )
import Sector
import Goods
import Trading
import System.Exit (exitFailure)
import System.Environment
import Data.Foldable (for_)
import Data.Maybe (isNothing, fromJust)
import Data.Ord
import Data.List

exitOnNothing :: Maybe a -> IO a
exitOnNothing Nothing = exitFailure
exitOnNothing (Just x) = return x

goodType :: Integer
goodType = 12
theGood = getGoodByID goodType

getAllJust :: [Maybe a] -> [a]
getAllJust [] = []
getAllJust [Nothing] = []
getAllJust [Just x] = [x]
getAllJust (x:xs) = (getAllJust [x]) ++ (getAllJust xs)

allPairs :: (Eq a) => [a] -> [a] -> [(a,a)]
allPairs xs ys = [(x,y) | x <-xs, y<-ys]

printRouteProfitability :: (Fractional a, Show a) => ((Sector, Sector), a) -> IO ()
printRouteProfitability (ports, profit) = do
  let buyFrom = (num . fst) ports
  let sellTo = (num . snd) ports
  putStr . show $ buyFrom
  putStr " -> "
  putStr . show $ sellTo
  putStr " ("
  putStr . show $ profit
  putStrLn " per good per turn)"

main :: IO ()
main = do
  (filename:_) <- getArgs
  f <- readFile filename
  ini <- exitOnNothing $ parseIni f
  let creontiSecs = getAllJust $ map (getSector ini) [50..95]
  let goodSellers = filter (`doesSell` theGood) creontiSecs
  let goodBuyers = filter (`doesBuy` theGood) creontiSecs
  let buySellPairs = allPairs goodSellers goodBuyers
  let profits = getAllJust $ map (\(buyFrom, sellTo) -> calculateProfitPerTurn buyFrom sellTo theGood) buySellPairs
  let ordered = reverse $ sortBy (comparing snd) (zip buySellPairs profits)
  let best = head ordered
  putStr "Most profitable route for "
  putStr . show $ name theGood
  putStr " is "
  printRouteProfitability best

  putStrLn "\nOther options:"
  for_ (tail ordered) printRouteProfitability 
