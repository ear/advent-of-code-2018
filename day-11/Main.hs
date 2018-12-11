module Main where

import Data.Set ( Set )
import qualified Data.Set as S

import Data.Map.Strict ( Map, (!?) )
import qualified Data.Map.Strict as M

import Data.Ord
import Data.Foldable

input = 7139
-- input = 18

-- (1,1) ... (300,1)
-- ...          ...
-- (1,300) ... (300,300)
type Coord = (Int,Int)
type Power = Int
type Grid = Map Coord Power

onEdge (x,y) = x == 1 || x == 300 || y == 1 || y == 300

-- a square is described by its topleft coord and its size
outside size (x,y) = x < 1 || x + (size-1) > 300 || y < 1 || y + (size-1) > 300

square g size c
  | outside size c = Nothing
  | otherwise  = Just $ M.restrictKeys g (S.fromAscList cs)
  where
    (cx,cy) = c
    cs = [ (x,y) | x <- [cx..cx+size-1], y <- [cy..cy+size-1] ]

topleft = minimum . M.keys

--

power (x,y) = hundredsDigit (rackID * n) - 5
  where
    n = rackID * y + input
    rackID = x + 10

hundredsDigit n
  | n < 100 = 0
  | otherwise = truncate (fromIntegral n / 100) `mod` 10

--

grid :: Grid
grid = M.fromList [ (c, power c) | c <- coords ]

coords = [ (x,y) | x <- [1..300], y <- [1..300] ]

--

-- sizes = [1..300]
-- 
type Size = Int
-- 
-- powers :: Map (Size,Coord) Power



search :: Size -> Grid -> (Size, Grid)
search size g = maximumBy (comparing fst) $ ss
  where
    ss = [ (sum s, s) | Just s <- square grid size <$> coords ]

--

main = do
  print . topleft . snd . search 3 $ grid
  print . fmap topleft . maximumBy (comparing fst) $ [ search n grid | n <- [1..300] ]
