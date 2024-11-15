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
  let w = handleMaybeDir (Map.lookup "Warp" sector)
  let race = handleMaybeRace (Map.lookup "Port Race" sector)

  return $ Sector n u d l r w race (parseBuys sector) (parseSells sector)

  where handleMaybeDir :: Maybe String -> Maybe Sector
        handleMaybeDir (Just s) = getSector ini (read s)
        handleMaybeDir Nothing = Just Wall
        handleMaybeRace :: Maybe String -> Integer
        handleMaybeRace Nothing = 0
        handleMaybeRace (Just r) = read r

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
