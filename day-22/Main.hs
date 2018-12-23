{-# language ViewPatterns #-}

import Data.Foldable
import Data.Semigroup

import qualified Data.List as L
import qualified Data.Array.IArray as A

type Coord   = (Int,Int) -- (0,0) = top left, non-negative coords

type Erosion = Int

data Region  = Rocky | Wet | Narrow

-- Input

depth = 5355
target = (14,796)
-- test
--depth = 510
--target = (10,10)

part1 = getSum . foldMap (Sum . risk . fromErosion) $ cave

main = print $ part1

-- Cave

mouth = (0,0) :: Coord

cave :: A.Array Coord Int {- Erosion -}
cave = A.array (mouth,target) $
  [ ( (x,y), at x y ) | y <- [0..snd target], x <- [0..fst target] ]
    where
      -- Compute erosion at coordinate
      at x y | (x,y) == mouth || (x,y) == target = depth `mod` 20183
      at x 0 = ((x * 16807) + depth) `mod` 20183
      at 0 y = ((y * 48271) + depth) `mod` 20183
      at x y = (cave A.! (x,y-1) * cave A.! (x-1,y) + depth) `mod` 20183

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
        showRegion c = show . fromErosion $ cave A.! c

instance Show Region where
  showsPrec _ Rocky  = showChar '.'
  showsPrec _ Wet    = showChar '='
  showsPrec _ Narrow = showChar '|'
