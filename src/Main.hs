module Main where

import IniParse
import SectorMap ( getSector )
import Sector
import Goods
import Trading
import System.Exit (exitFailure)
import System.Environment
import Data.Foldable (for_)
import Data.Maybe (isNothing, isJust, fromJust)
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

allPairs :: [a] -> [b] -> [(a,b)]
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
  let portPairs = filter (\(port1, port2) -> (num port1) < (num port2)) $ allPairs goodSellers goodBuyers
  let goodPairs = allPairs allGoods allGoods
  let allCombos = allPairs portPairs goodPairs
  
  let profits = map (\((port1, port2), (good1, good2)) -> calculateTwoWayProfitPerTurn port1 port2 good1 good2) allCombos
  let ordered = reverse $ sortBy (comparing (fromJust . snd)) $ filter (isJust . snd) (zip allCombos profits)
  let best = head ordered
  putStrLn . show . snd $ best
  putStrLn . show . fst $ best
  putStrLn . show . fromJust $ calculateTwoWayProfitPerTurn (fromJust $ getSector ini 87) (fromJust $ getSector ini 74) (getGoodByID 8) (getGoodByID 12)
  -- putStr "Most profitable route for "
  -- putStr . show $ name theGood
  -- putStr " is "
  -- printRouteProfitability best

  -- putStrLn "\nOther options:"
  -- for_ (tail ordered) printRouteProfitability 
