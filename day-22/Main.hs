{-# language ViewPatterns #-}
{-# language RecordWildCards #-}
{-# language FlexibleContexts #-}

import Text.Printf

import Data.Foldable
import Data.Semigroup

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Array.IArray as A

type Coord   = (Int,Int) -- (0,0) = top left, non-negative coords

type Erosion = Int

data Region  = Rocky | Wet | Narrow
  deriving (Eq, Enum)

type Cave = A.Array Coord Region


-- Input

depth = 5355
target = (14,796)
-- test
--depth = 510
--target = (10,10)

(xM,yM) = (26,800)

part1 = getSum . foldMap (Sum . risk) $ cave

part2 = minimum [tT, tN, tG]
  where
    tT = visits M.! Torch
    tN = pad 7 $ visits M.! Neither
    tG = pad 7 $ visits M.! Gear
    visits = (v_ flood) A.! target
    pad n x | x == maxBound = x
            | otherwise     = x + n

main = do
  print $ part1
  print $ part2

t = True
f = False

-- Flood

type Time = Int

data Tool = Neither | Torch | Gear
  deriving (Eq, Enum, Ord, Show)

type Item = (Time, Tool, Coord)

type Visit = (M.Map Tool Int)

data Flood = Flood
  { t_ :: Time
  , v_ :: A.Array Coord Visit
  , q_ :: S.Set Item
  } deriving Show

emptyVisit :: Visit
emptyVisit = M.fromAscList $
  [ (Neither,maxBound)
  , (Torch,maxBound)
  , (Gear,maxBound) ]

emptyFlood :: Cave -> Flood
emptyFlood a = Flood
  { t_ = 0
  , v_ = A.listArray (A.bounds a) $
           M.insert Torch 0 emptyVisit : repeat emptyVisit
  , q_ = S.singleton (0,Torch,(0,0)) }

flood :: Flood
flood = head . dropWhile (not . S.null . q_) . iterate flood1 . emptyFlood $ cave

flood1 :: Flood -> Flood
flood1 f@Flood{..} =
  case S.minView q_ of
    Nothing -> f
    Just (i@(time,_,_), rest) -> f'
      where
        f' = f { t_ = t_ + 1, v_ = v', q_ = q' }
        -- xs = frontier to expand
        -- ys = later
        (now,later) = S.partition (\(time',_,_) -> time == time') q_
        -- zs = new frontier
        frontier = adjs f =<< toList now
        -- save the tools in the map
        v' = A.accum keepLeast v_ [ (c,(t,tool)) | (t,tool,c) <- frontier ]

        keepLeast v (t,tool) = M.insertWith min tool t v

        q' = S.union later $ S.fromList frontier

adjs :: Flood -> Item -> [Item]
adjs f i@(_,_,c) = itemAt f i =<< around c

itemAt :: Flood -> Item -> Coord -> [Item]
itemAt Flood{..} i@(time,tool,c) c'
  | time' < (v_ A.! c') M.! tool' = [i]
  | otherwise                     = []
  where
    i@(time',tool',_)
      | compatible tool c' = (time + 1, tool          , c')
      | otherwise          = (time + 8, pick tool r r', c')
    r  = cave A.! c
    r' = cave A.! c'

compatible :: Tool -> Coord -> Bool
compatible t c = fromEnum t /= fromEnum (cave A.! c)

pick :: Tool -> Region -> Region -> Tool
pick t r1 r2
  | r1 == r2 = t
  | r1 /= r2 = toEnum . head
             . L.delete (fromEnum r1)
             . L.delete (fromEnum r2) $ [0,1,2]

-- TODO: check if the ordering of the coordinates is sane
around :: Coord -> [Coord]
around c = filter inside [ n c, e c, s c, w c ]
  where inside (x,y) = 0 <= x && x <= xM && 0 <= y && y <= yM


-- Cave

cave = fromErosion <$> erosion

-- Erosion

mouth = (0,0) :: Coord

erosion :: A.Array Coord Int
erosion = A.array (mouth,(xM,yM)) $
  [ ( (x,y), at x y ) | y <- [0..yM], x <- [0..xM] ]
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


-- Movements

n, e, s, w :: Coord -> Coord
n (x,y) = (x,y-1)
e (x,y) = (x+1,y)
s (x,y) = (x,y+1)
w (x,y) = (x-1,y)


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

pf Flood{..} = print q_

--pf f = putStrLn . L.intercalate "\n" $
--  [ L.intercalate " " [ showDists f (x,y) | x <- [0..fst target] ] | y <- [0..snd target] ]
--  where showDists f (x,y) =
--          let T{..} = f A.! (x,y) in L.intercalate "" $ map showDist [g_,t_,n_]
--        showDist Nothing  = " ∞"
--        showDist (Just n) = printf "%2d" n
