module GameOfLife (
  Grid,
  Width (..),
  Height (..),
  height,
  width,
  mkGrid,
  Pattern,
  mkPattern,
  add,
  step,
  Row (..),
  Col (..),
  isOccupied,
  Coord (..),
  translate,
) where

import Data.Aeson
import Data.Set (Set)
import Data.Set qualified as Set

newtype Width = Width {fromWidth :: Int}
  deriving (Num)
newtype Height = Height {fromHeight :: Int}
  deriving (Num)

newtype Row = Row {fromRow :: Int}
  deriving (Eq, Ord, Num, Show)

newtype Col = Col {fromCol :: Int}
  deriving (Eq, Ord, Num, Show)

newtype Coord = Coord {fromCoord :: (Row, Col)}
  deriving (Eq, Ord, Show)

instance FromJSON Coord where
  parseJSON = withObject "Coord" \obj ->
    (\r c -> Coord (Row r, Col c)) <$> (obj .: "row") <*> (obj .: "col")

instance ToJSON Coord where
  toJSON :: Coord -> Value
  toJSON (Coord (Row r, Col c)) = object ["row" .= r, "col" .= c]

data Grid = Grid
  { size :: (Width, Height)
  , occupied :: Set Coord
  }

width :: Grid -> Width
width = fst . size

height :: Grid -> Height
height = snd . size

isOccupied :: Grid -> Coord -> Bool
isOccupied grid coord = Set.member coord grid.occupied

mkGrid :: Width -> Height -> [Coord] -> Grid
mkGrid w h occupied = Grid{size = (w, h), occupied = Set.fromList occupied}

newtype Pattern = Pattern [Coord]

isInBounds :: Grid -> Coord -> Bool
isInBounds (Grid (Width w, Height h) _) (Coord (Row row, Col col)) =
  row >= 0 && row < h && col >= 0 && col < w

add :: Pattern -> Grid -> Grid
add (Pattern pattern) grid =
  let toInsert = Set.fromList $ filter (isInBounds grid) pattern
   in grid{occupied = Set.union grid.occupied toInsert}

mkPattern :: [Coord] -> Pattern
mkPattern = Pattern

coords :: Grid -> [Coord]
coords (Grid (Width w, Height h) _) =
  (\r c -> Coord (Row r, Col c)) <$> [0 .. h - 1] <*> [0 .. w - 1]

step :: Grid -> Grid
step grid =
  grid
    { occupied =
        Set.fromList $
          filter (isLive grid) $
            coords grid
    }

isLive :: Grid -> Coord -> Bool
isLive grid coord =
  let isAlive = Set.member coord grid.occupied
      liveNeighbors = filter (`Set.member` grid.occupied) $ neighbors grid coord
   in case length liveNeighbors of
        2 -> isAlive
        3 -> True
        _ -> False

neighbors :: Grid -> Coord -> [Coord]
neighbors grid (Coord (row, col)) =
  filter (isInBounds grid) $
    Coord
      <$> [ (row - 1, col - 1)
          , (row - 1, col)
          , (row - 1, col + 1)
          , (row, col - 1)
          , (row, col + 1)
          , (row + 1, col - 1)
          , (row + 1, col)
          , (row + 1, col + 1)
          ]

translate :: Int -> Int -> Pattern -> Pattern
translate x y (Pattern cs) = Pattern $ (\(Coord (row, col)) -> Coord (row + Row (y - 1), col + Col (x - 1))) <$> cs