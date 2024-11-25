module SectorMap (
  Sectors
  ,getSectorMap
  ,getSectorKeys
  ,getSector
) where

import IniParse
import qualified Data.Map as Map
-- import Data.Text (isInfixOf)
import Data.List (isInfixOf)
import Data.Maybe
import Sector ( Sector(Sector, Wall) )
import Goods

type Sectors = [Maybe Sector]
-- the sector map gives us a way of memoising the generated sectors
-- it stores an infinite list of sectors loaded from the ini file, but
-- it passes itself into getSectorFromIni. All cyclical references are
-- references to items in the sector map, rather than further calls of
-- getSectorFromIni. This way, getSectorFromIni should only be called
-- once for any given sector, but still should not be called until the
-- first time that sector is used.
getSectorMap :: Ini -> Sectors
getSectorMap ini = sectorMap
  where sectorMap = [getSectorFromIni sectorMap ini n | n <- [0..]]

getSector :: Sectors -> Integer -> Maybe Sector
getSector m i = m !! fromIntegral i

getSectorKeys :: Ini -> [String]
getSectorKeys ini = filter (isInfixOf "Sector=") (Map.keys ini)

getSectorFromIni :: Sectors -> Ini -> Integer -> Maybe Sector
getSectorFromIni m ini n = do
  let sectorKey = "Sector=" ++ show n
  sector <- Map.lookup sectorKey ini
  let u = handleMaybeDir (Map.lookup "Up" sector)
  let d = handleMaybeDir (Map.lookup "Down" sector)
  let l = handleMaybeDir (Map.lookup "Left" sector)
  let r = handleMaybeDir (Map.lookup "Right" sector)
  let w = handleMaybeDir (Map.lookup "Warp" sector)
  let race = handleMaybeRace (Map.lookup "Port Race" sector)
  let portLvl = handleMaybePort (Map.lookup "Port Level" sector)

  return $ Sector n u d l r w race portLvl (parseBuys sector) (parseSells sector)

  where handleMaybeDir :: Maybe String -> Maybe Sector
        handleMaybeDir (Just s) = getSector m (read s)
        handleMaybeDir Nothing = Just Wall
        handleMaybeRace :: Maybe String -> Integer
        handleMaybeRace = read . fromMaybe "0"
        handleMaybePort :: Maybe String -> Integer
        handleMaybePort = read . fromMaybe "0"

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
