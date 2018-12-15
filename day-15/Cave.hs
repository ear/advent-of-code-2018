{-# language RecordWildCards #-}
{-# language ViewPatterns #-}

module Cave
  ( Coord(..)
  , Cave
  , fromString
  ) where

import Data.Maybe
import Data.Bifunctor
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Array.IArray as A

data Coord = C Int Int -- y x
  deriving (Show, Eq, Ord)

-- Directions, in "reading order"
data Dir = N | W | E | S deriving (Show, Eq, Ord)

data Race = Goblin | Elf deriving Show

data Unit = Unit
  { uRace_ :: Race
  } deriving Show

data Cave = Cave
  { cW_ {- ^ width  -} :: Int
  , cH_ {- ^ height -} :: Int
  , cS_ {- ^ spaces -} :: (S.Set Coord)
  , cU_ {- ^ units  -} :: (M.Map Coord Unit)
  } deriving Show

fromString :: String -> Cave
fromString xs = Cave w h s m
  where
    yxs = map (second $ zip [0..]) (zip [0..] (lines xs))
    h = length yxs
    w = length (snd $ head yxs)

    -- Walls '#' and open squares '.'
    inanimate = map (second $ map $ second removeUnit) yxs
      where
        removeUnit 'G' = '.'
        removeUnit 'E' = '.'
        removeUnit '.' = '.'
        removeUnit '#' = '#'
        removeUnit _ = error "spurious character in cave string"

    -- Set of open squares '.'
    s :: S.Set Coord
    s = S.fromList [ C y x | (y,xs) <- inanimate, (x,'.') <- xs ]

    -- Goblins and Elves
    beings = map (second $ map $ second keepUnit) yxs
      where
        keepUnit 'G' = Just $ Unit Goblin
        keepUnit 'E' = Just $ Unit Elf
        keepUnit  _  = Nothing

    -- Map of units
    m = M.fromList [ (C y x, unit) | (y,xs) <- beings, (x,Just unit) <- xs ]

-- | Coords adjacent to a given one (north, east, south, west.)
adjs (C y x) = [C (y-1) x, C y (x+1), C (y+1) x, C y (x-1)]

showCave :: Cave -> String
showCave cave@Cave{..} = L.intercalate "\n"
  [ [ showTile cave (C y x) | x <- [0..cW_-1] ] | y <- [0..cH_-1] ]

showTile :: Cave -> Coord -> Char
showTile Cave{..} c | Just Unit{..} <- cU_ M.!? c =
  case uRace_ of
    Goblin -> 'G'
    Elf    -> 'E'
showTile Cave{..} c | c `S.member` cS_ = '.'
showTile _ _ = '#'




