module SectorMap (
  getSectorKeys
  ,getSector
) where

import IniParse
import qualified Data.Map as Map
-- import Data.Text (isInfixOf)
import Data.List (isInfixOf)
import Sector

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

  return $ Sector n u d l r 0 [] []

  where handleMaybeDir :: Maybe String -> Maybe Sector
        handleMaybeDir (Just s) = getSector ini (read s)
        handleMaybeDir Nothing = Just Wall
