{-# language ViewPatterns #-}
{-# language RecordWildCards #-}
{-# language FlexibleContexts #-}

import Text.Printf

import Data.Foldable
import Data.Semigroup

import qualified Data.List as L
import qualified Data.Array.IArray as A

type Coord   = (Int,Int) -- (0,0) = top left, non-negative coords

type Erosion = Int

data Region  = Rocky | Wet | Narrow

type Cave = A.Array Coord Region


-- Input

--depth = 5355
--target = (14,796)
-- test
depth = 510
target = (10,10)

part1 = getSum . foldMap (Sum . risk) $ cave

main = print $ part1

-- Flood
--
-- Idea: flood the cave with a triple of `Maybe Int`s
--
-- Each represents what is the minimum known distance from the cave's mouth
-- such that a path of that length arrives to the coordinates with that tool.
--
-- Starts out as Nothings and expands successively.

data T = T { g_ :: Maybe Int, t_ :: Maybe Int, n_ :: Maybe Int }

type Flood = A.Array Coord T

emptyFlood :: Cave -> Flood
emptyFlood a = A.listArray (A.bounds a) $
  (T Nothing (Just 0) Nothing) : repeat (T Nothing Nothing Nothing)

flood :: Cave -> Flood
flood cave = flood1 cave (emptyFlood cave) [mouth]

flood1 :: Cave -> Flood -> [Coord] -> Flood
flood1 = undefined


-- Cave

cave = fromErosion <$> erosion

-- Erosion

mouth = (0,0) :: Coord

erosion :: A.Array Coord Int
erosion = A.array (mouth,target) $
  [ ( (x,y), at x y ) | y <- [0..snd target], x <- [0..fst target] ]
    where
      -- Compute erosion at coordinate
      at x y | (x,y) == mouth || (x,y) == target = depth `mod` 20183
      at x 0 = ((x * 16807) + depth) `mod` 20183
      at 0 y = ((y * 48271) + depth) `mod` 20183
      at x y = (erosion A.! (x,y-1) * erosion A.! (x-1,y) + depth) `mod` 20183

risk :: Region -> Int
risk Rocky  = 0
risk Wet    = 1
risk Narrow = 2

fromErosion :: Int -> Region
fromErosion ((`mod` 3) -> c) | c == 0 = Rocky | c == 1 = Wet | c == 2 = Narrow

-- Debug

p = putStrLn . L.intercalate "\n" $
  [ concat [ showRegion (x,y) | x <- [0..fst target] ] | y <- [0..snd target] ]
  where showRegion (0,0) = "M"
        showRegion c | c == target = "T"
        showRegion c = show . fromErosion $ erosion A.! c

instance Show Region where
  showsPrec _ Rocky  = showChar '.'
  showsPrec _ Wet    = showChar '='
  showsPrec _ Narrow = showChar '|'

pf f = putStrLn . L.intercalate "\n" $
  [ L.intercalate " " [ showDists f (x,y) | x <- [0..fst target] ] | y <- [0..snd target] ]
  where showDists f (x,y) =
          let T{..} = f A.! (x,y) in L.intercalate "" $ map showDist [g_,t_,n_]
        showDist Nothing  = " ∞"
        showDist (Just n) = printf "%2d" n
