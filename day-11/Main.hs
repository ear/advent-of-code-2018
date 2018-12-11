module Main where

import Data.Set ( Set )
import qualified Data.Set as S

import Data.Map.Strict ( Map, (!?) )
import qualified Data.Map.Strict as M

import Data.Ord
import Data.Foldable

input = 7139
-- input = 42

-- (1,1) ... (300,1)
-- ...          ...
-- (1,300) ... (300,300)
type Coord = (Int,Int)
type Power = Int
type Grid = Map Coord Power

onEdge (x,y) = x == 1 || x == 300 || y == 1 || y == 300

square g c
  | onEdge c   = Nothing
  | otherwise  = Just $ M.restrictKeys g (S.fromAscList cs)
  where
    (cx,cy) = c
    cs = [ (x,y) | x <- [cx-1,cx,cx+1], y <- [cy-1,cy,cy+1] ]

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

search :: Grid -> Coord
search g = topleft . fst . maximumBy (comparing snd) $ ss
  where
    ss = [ (s, sum s) | Just s <- square grid <$> coords ]

main = do
  print $ search grid
