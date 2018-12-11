module Main where

import Data.Set ( Set )
import qualified Data.Set as S

import Data.Map.Strict ( Map, (!) )
import qualified Data.Map.Strict as M

import Data.Ord
import Data.Foldable
import qualified Data.List as L

input = 7139
-- input = 18
-- input = 42

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

sizes = [1..300]

type Size = Int

powers :: Map (Size,Coord) Power
powers = L.foldl' go size1 (tail sizes)
  where
    size1 = M.mapKeys (\c -> (1,c)) grid
    -- compute all the powers for a given size
    -- by breaking in half and using the previous half-size squares
    go m s
      | even s = -- use 4 even-sized ones
          let s2 = floor (fromIntegral s / 2)
              -- subsquares
              ss = [ ( (x,y)
                     , sum $ map (m !) $ map (\c->(s2,c)) [ (x,y), (x+s2,y), (x,y+s2), (x+s2,y+s2) ] )
                   | (x,y) <- inside ]
          in (M.fromList $ map (\(c,p) -> ((s,c),p)) ss) `M.union` m
      | otherwise = -- use an even one, and many 1-sized ones
          let se = s-1 -- size of the even one
              ss = [ ( c , (m ! (se,c)) + sum (map (m !) (extra c)) ) | c <- inside ]
              extra (x,y) = map (\c -> (1,c)) $ concat
                [ [ (x+s-1,y') | y' <- [y..y+s-2] ] -- right edge
                , [ (x',y+s-1) | x' <- [x..x+s-2] ] -- bottom edge
                , [ (x+s-1,y+s-1) ] ]
          in (M.fromList $ map (\(c,p) -> ((s,c),p)) ss) `M.union` m
      where
        inside = filter (not . outside s) coords

--

search :: Size -> Grid -> (Size, Grid)
search size g = maximumBy (comparing fst) $ ss
  where
    ss = [ (sum s, s) | Just s <- square grid size <$> coords ]

--

main = do
  print . topleft . snd . search 3 $ grid
  --print . fmap topleft . maximumBy (comparing fst) $ [ search n grid | n <- [1..300] ]
  let maxpower = maximum powers
  print maxpower
  print . L.find ((maxpower ==) . snd) . M.toList $ powers
