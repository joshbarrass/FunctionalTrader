module Main where

import IniParse
import SectorMap ( getSector )
import Sector (distanceIndex)
import Goods (getGoodByID)
import System.Exit (exitFailure)
import System.Environment

exitOnNothing :: Maybe a -> IO a
exitOnNothing Nothing = exitFailure
exitOnNothing (Just x) = return x

goodType = 4

main :: IO ()
main = do
  (filename:_) <- getArgs
  f <- readFile filename
  ini <- exitOnNothing $ parseIni f
  sec <- exitOnNothing $ getSector ini 83
  print sec
  putStr $ "Distance index for Good " ++ show goodType ++ ": "
  distanceIndex <- exitOnNothing $ distanceIndex sec (getGoodByID goodType)
  print distanceIndex
