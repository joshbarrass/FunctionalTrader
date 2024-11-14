module IniParse (
  Ini
  ,parseIni
                ) where

-- Credit to https://lpraz.github.io/parsing/haskell/2020/12/19/haskell-readp-parse-ini-config.html
-- for most of this code

import Text.ParserCombinators.ReadP
    ( char,
      choice,
      eof,
      get,
      look,
      manyTill,
      pfail,
      readP_to_S,
      satisfy,
      ReadP )
import Data.Maybe (catMaybes)
import qualified Data.Map as Map

type Ini = Map.Map String (Map.Map String String)

-- define an EOL character to be discarded
-- Either matches EOF (provided by ReadP) or a newline
eol :: ReadP ()
eol = choice [char '\n' >> return (), eof]

-- define a parser for a single Key/Val pair of the ini
-- this reads as a key until we hit an equals sign, ignoring equals
-- signs, semicolons or newlines, then reads the rest of the line as
-- the val
iniKeyVal :: ReadP (String, String)
iniKeyVal = do
  key <- manyTill (satisfy (flip notElem "=;\n")) (char '=')
  val <- manyTill get eol
  return (key, val)

iniSectionName :: ReadP String
iniSectionName = do
  char '['
  sectionName <- manyTill (satisfy (flip notElem "\n")) (char ']')
  eol
  return sectionName

iniIgnoreLine :: ReadP ()
iniIgnoreLine = choice [iniComment, iniBlankLine]

iniComment :: ReadP ()
iniComment = do
    char ';'
    manyTill get eol
    return ()

iniBlankLine :: ReadP ()
iniBlankLine = do
    manyTill (satisfy (flip elem " \t")) eol
    return ()

iniKeys :: ReadP (Map.Map String String)
iniKeys = do
    maybeKeys <- manyTill (choice
        [ iniKeyVal >>= (return . Just)
        , iniIgnoreLine >> return Nothing
        ])
        lookEndOfSection
    let keys = catMaybes maybeKeys
    return $ Map.fromList keys

lookEndOfSection :: ReadP ()
lookEndOfSection = do
    rest <- look
    if (rest == []) || (head rest == '[') then return () else pfail

iniSection :: ReadP (String, Map.Map String String)
iniSection = do
    sectionName <- iniSectionName
    keys <- iniKeys
    return (sectionName, keys)

ini :: ReadP (Map.Map String (Map.Map String String))
ini = do
  -- read and discard any keys that came before the first section
  keys <- iniKeys
  sections <- manyTill iniSection eof
  return $ Map.fromList sections

handleParseResult :: [(a, String)] -> Maybe a
handleParseResult [(result, _)] = Just result
handleParseResult [] = Nothing
handleParseResult _ = Nothing

parseIni :: String -> Maybe (Ini)
parseIni contents = do
  let parser = readP_to_S (ini <* eof)
  let result = parser contents
  handleParseResult result

