module SectorMap (
  getSectorKeys
  ,getSector
) where

import IniParse
import qualified Data.Map as Map
-- import Data.Text (isInfixOf)
import Data.List (isInfixOf)
import Sector ( Sector(Sector, Wall) )
import Goods

getSectorKeys :: Ini -> [String]
getSectorKeys ini = filter (isInfixOf "Sector=") (Map.keys ini)

getSector :: Ini -> Integer -> Maybe Sector
getSector ini n = do
  let sectorKey = "Sector=" ++ show n
  sector <- Map.lookup sectorKey ini
  let u = handleMaybeDir (Map.lookup "Up" sector)
  let d = handleMaybeDir (Map.lookup "Down" sector)
  let l = handleMaybeDir (Map.lookup "Left" sector)
  let r = handleMaybeDir (Map.lookup "Right" sector)

  return $ Sector n u d l r 0 (parseBuys sector) (parseSells sector)

  where handleMaybeDir :: Maybe String -> Maybe Sector
        handleMaybeDir (Just s) = getSector ini (read s)
        handleMaybeDir Nothing = Just Wall

splitByComma :: String -> [String]
splitByComma "" = []
splitByComma s = case break (== ',') s of
    (before, "")     -> [before]
    (before, _:rest) -> before : splitByComma rest


parseGoods :: String -> [Good]
parseGoods = map (getGoodByID . read) . splitByComma

parseBuys :: Map.Map String String -> [Good]
parseBuys sector
  | "Buys" `Map.member` sector = parseGoods (sector Map.! "Buys")
  | otherwise = []

parseSells :: Map.Map String String -> [Good]
parseSells sector
  | "Sells" `Map.member` sector = parseGoods (sector Map.! "Sells")
  | otherwise = []
