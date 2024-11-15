module Main where

import IniParse
import SectorMap ( getSector )
import Sector
import Goods (getGoodByID)
import System.Exit (exitFailure)
import System.Environment
import Data.Foldable (for_)
import Data.Maybe (isNothing, fromJust)

exitOnNothing :: Maybe a -> IO a
exitOnNothing Nothing = exitFailure
exitOnNothing (Just x) = return x

goodType = 12

getNarcDistance :: Maybe Sector -> IO ()
getNarcDistance Nothing = return ()
getNarcDistance (Just sec) = do
  if (getGoodByID goodType) `notElem` (sells sec) then return () else do
    let di = distanceIndex sec (getGoodByID goodType)
    if isNothing di then return () else do
      putStr "Sector: "
      print $ num sec
      putStr $ "Distance index for Narc: "
      print $ fromJust di

main :: IO ()
main = do
  (filename:_) <- getArgs
  f <- readFile filename
  ini <- exitOnNothing $ parseIni f
  let creontiSecs = map (getSector ini) [50..95]
  for_ creontiSecs getNarcDistance
  
