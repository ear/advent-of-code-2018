{-# language RecordWildCards #-}
{-# language ViewPatterns #-}
{-# language OverloadedLists #-}

module Cave
  ( Coord(..)
  , Cave
  , fromString
  ) where

import Debug.Trace

import Data.Maybe
import Data.Bifunctor
import qualified Data.Set          as S
import qualified Data.Map.Strict   as M
import qualified Data.List         as L
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
adjs :: Coord -> [Coord]
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

-- | DD: Distance & Direction
-- The Ord instance lets use use `minimum` to pick between the closest
-- targets the one whose path begins with a step in the least "reading
-- order" direction.
data DD = DD Int Dir
  deriving (Show, Eq, Ord)

-- | Queue of coordinate sorted by Directional Distances
newtype Squares = SQ (M.Map DD Coord)
  deriving (Show)

-- | Map of tiny cute little arrows
type Flood = (M.Map Coord DD)

-- | Compute the coordinates of the next squares to flood
-- is it true that one just needs the immediate previous level?
-- no, one needs the set of all seen coordinates too
--flood1 :: Cave -> Flood -> Squares -> (Flood,Squares)
--flood1 cave@Cave{..} f (SQ cs) = (f',(SQ cs'))
--  where
--
--    f' = new `M.union` f
--    new = M.fromList [ (c,dd) | (dd,c) <- frontier ]
--    frontier = M.toAscList cs
--
--    cs' = M.fromList [ (DD (succ dist) dir,c')
--                     | (DD dist dir,c) <- frontier
--                     , (_,c') <- openNeighbours c ]
--    openNeighbours = traceShowId . filter ((`M.notMember` f) . snd) . neighbours cave

-- | Directional Squares - open squares organized per-direction (in reading order)
data DS = DS Squares Squares Squares Squares
  deriving (Show)

dequeue :: DS -> ([(DD,Coord)],[(DD,Coord)],[(DD,Coord)],[(DD,Coord)])
dequeue (DS (SQ ns) (SQ ws) (SQ es) (SQ ss))
  = (dq ns, dq ws, dq es, dq ss)
  where
    dq = M.toAscList

--enqueue :: [(DD,Coord)] -> [(DD,Coord)] -> [(DD,Coord)] -> [(DD,Coord)] -> DS
enqueue ns ws es ss
  = (DS (SQ $ eq 'N' ns) (SQ $ eq 'W' ws) (SQ $ eq 'E' es) (SQ $ eq 'S' ss))
  where
    eq dir xs = M.fromList . reverse . traceShow (dir,xs) $ xs

-- ^ these are nuts

flood1 :: Cave -> Flood -> DS -> (Flood,DS)
flood1 cave@Cave{..} f (dequeue -> (ns,ws,es,ss))
  = (f',enqueue ns' ws' es' ss')
  where
    f' = M.unions [f,fn,fw,fe,fs]
    (fn,fw,fe,fs) = (toF ns, toF ws, toF es, toF ss)
    toF x = M.fromList [ (c,dd) | (dd,c) <- x ]

    (ns',ws',es',ss') = (adjs ns, adjs ws, adjs es, adjs ss)
    adjs x = [ (DD (dist+1) dir, c')
             | (DD  dist    dir, c ) <- x
             , (_,c') <- openNeighbours c ]
    openNeighbours = filter ((`M.notMember` f) . snd) . neighbours cave

--   C 24 16   n'bours
-- ^ C 23 16 : 22,16 23,15 23,17 24,16
-- < C 24 15 : 
-- > C 24 17 : 
-- v C 25 16 : 24,16 25,15 25,17 26,16 -- ok correct
--
-- next the filter


p cave n = mapM_ (putStrLn . showFlood cave) . take 1 . drop (n-1) . flood cave $ C 24 16
-- | Neighbours of a given square
neighbours :: Cave -> Coord -> [(Dir,Coord)]
neighbours cave@Cave{..} (C y x) = filter (isOpen cave . snd)
  [ (N, C (y-1) x), (W, C y (x-1)), (E, C y (x+1)), (S, C (y+1) x) ]

-- | Is the given square open? '.'
isOpen :: Cave -> Coord -> Bool
isOpen Cave{..} c = c `S.member` cS_

--flood :: Cave -> Coord -> [Flood]
--flood cave c = go flood0 squares0
--  where
--    squares0 = SQ . M.fromList
--             . map (\(dir,c) -> (DD 1 dir,c)) . neighbours cave $ c
--    flood0 = M.singleton c (DD 0 N)
--    go f s = f' : go f' s'
--      where
--        (f',s') = flood1 cave f s

flood :: Cave -> Coord -> [Flood]
flood cave c = flood0 : go flood0 squares0
  where
    flood0 = M.singleton c (DD 0 N)
    squares0 = enqueue [n] [w] [e] [s]
    [n,w,e,s] = map (\(dir,c) -> (DD 1 dir,c)) . neighbours cave $ c
    go f s = f' : go f' s'
      where
        (f',s') = flood1 cave f s

showFlood :: Cave -> Flood -> String
showFlood cave@Cave{..} f = L.intercalate "\n"
  [ [ showTile' cave f (C y x) | x <- [0..cW_-1] ] | y <- [0..cH_-1] ]

showTile' :: Cave -> Flood -> Coord -> Char
showTile' Cave{..} f c | Just (DD dist dir) <- f M.!? c =
    head . show $ dist
showTile' Cave{..} _ c | Just Unit{..} <- cU_ M.!? c =
  case uRace_ of
    Goblin -> 'G'
    Elf    -> 'E'
showTile' Cave{..} _ c | c `S.member` cS_ = '.'
showTile' _        _ _ = '#'

