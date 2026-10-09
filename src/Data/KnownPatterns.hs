{-# LANGUAGE DuplicateRecordFields #-}

module Data.KnownPatterns (
  Patterns,
  loadPatterns,
  sections,
  searchForPattern,
  width,
  coords,
  toPattern,
) where

import Control.Monad (join)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as TIO
import Data.Void (Void)
import GameOfLife (Col (..), Coord (..), Pattern, Row (..), mkPattern)
import Text.Megaparsec (MonadParsec (takeWhileP), Parsec, many, parseMaybe, some, (<|>))
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Char qualified as C

type Parser = Parsec Void Text

newtype Patterns = Patterns (Map Text KnownPattern)

data KnownPattern = KnownPattern
  { section :: SectionName
  , width :: Int
  , coords :: [(Bool, Coord)]
  }
  deriving (Show)

type SectionName = Text
type PatternName = Text

loadPatterns :: FilePath -> IO Patterns
loadPatterns filePath = do
  dataFile <- TIO.readFile filePath
  let results = parseMaybe fileParser dataFile
  pure $ Patterns $ fromMaybe Map.empty results

fileParser :: Parser (Map PatternName KnownPattern)
fileParser = do
  Map.fromList . join <$> many sectionParser

sections :: Patterns -> [(SectionName, [PatternName])]
sections (Patterns patterns) =
  Map.toList
    $ foldr
      ( \(patternName, pattern) results ->
          Map.insertWith (++) pattern.section [patternName] results
      )
      Map.empty
    $ Map.toList patterns

searchForPattern :: Patterns -> PatternName -> Maybe KnownPattern
searchForPattern (Patterns patterns) patternName = Map.lookup patternName patterns

restOfLine :: Parser Text
restOfLine = takeWhileP (Just "line content") (\c -> c /= '\n' && c /= '\r') <* C.eol

sectionParser :: Parser [(PatternName, KnownPattern)]
sectionParser = do
  sectionName <- string "section: " *> restOfLine
  patterns <- many (knownPatternParser sectionName <* C.eol)
  _ <- C.eol
  pure patterns

knownPatternParser :: SectionName -> Parser (PatternName, KnownPattern)
knownPatternParser sectionName = do
  patternName <- string "name: " *> restOfLine
  _ <- string "pattern:" >> restOfLine
  spaces <- some coordLineParser
  let (width, coords) = convertToCoordsAndWidth spaces
  pure (patternName, KnownPattern sectionName width coords)

coordLineParser :: Parser [Bool]
coordLineParser = do
  some ((False <$ char '_') <|> (True <$ char '.')) <* C.eol

convertToCoordsAndWidth :: [[Bool]] -> (Int, [(Bool, Coord)])
convertToCoordsAndWidth spaces =
  let coords = do
        (rowId, row) <- zip [0 ..] spaces
        (colId, occupied) <- zip [0 ..] row
        pure (occupied, Coord (Row rowId, Col colId))
      width = foldr (\(_, Coord (_, Col col)) m -> max col m) (-1) coords
   in (width + 1, coords)

toPattern :: KnownPattern -> Pattern
toPattern known = mkPattern $ snd <$> filter fst known.coords