{-# language ViewPatterns #-}

import qualified Data.List as L

-- input
--depth = 5355
--target = (14,796)
-- test
depth = 510
target = (10,10)

main = putStrLn . L.intercalate "\n" $
  [ concat [ showRegion (x,y) | x <- [0..15] ] | y <- [0..15] ]

showRegion (0,0) = "M"
showRegion c | c == target = "T"
showRegion c = show $ at c

type Coord = (Int,Int) -- (0,0) = top left, non-negative coords

data Region = Rocky | Wet | Narrow

instance Show Region where
  showsPrec _ Rocky  = showChar '.'
  showsPrec _ Wet    = showChar '='
  showsPrec _ Narrow = showChar '|'

risk :: Region -> Int
risk Rocky  = 0
risk Wet    = 1
risk Narrow = 2

at :: Coord -> Region
at ((`mod` 3) . erosion -> c)
  | c == 0 = Rocky
  | c == 1 = Wet
  | c == 2 = Narrow

geo :: Coord -> Int
geo c | c == (0,0) || c == target = 0
geo (x,0) = x * 16807
geo (0,y) = y * 48271
geo (x,y) = erosion (x-1,y) * erosion (x,y-1)

erosion :: Coord -> Int
erosion = (`mod` 20183) . (+ depth) . geo

