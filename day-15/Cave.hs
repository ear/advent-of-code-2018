{-# language RecordWildCards #-}
{-# language ViewPatterns #-}
{-# language OverloadedLists #-}

module Cave
  ( Coord(..)
  , Cave
  , fromString
  , p
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
  [ concat [ showTile cave (C y x) | x <- [0..cW_-1] ] | y <- [0..cH_-1] ]

showTile :: Cave -> Coord -> String
showTile Cave{..} c | Just Unit{..} <- cU_ M.!? c =
  case uRace_ of
    Goblin -> "\x1b[0;31mG\x1b[0m"
    Elf    -> "\x1b[0;32mE\x1b[0m"
showTile Cave{..} c | c `S.member` cS_ = " "
showTile _ _ = "░"

-- | Directions, in "reading order"
data Dir = N | W | E | S deriving (Show, Eq, Ord)

-- | DD: Distance & Direction
-- The Ord instance lets use use `minimum` to pick between the closest
-- targets the one whose path begins with a step in the least "reading
-- order" direction.
data DD = DD Int Dir
  deriving (Show, Eq, Ord)

-- | Map of distances and directions
type Flood = (M.Map Coord DD)

-- | Frontiers of 4-way parallel scan in the four directions
type Frontiers = ([Coord],[Coord],[Coord],[Coord])

flood :: Cave -> Coord -> [Flood]
flood cave@Cave{..} c@(C y x) = fl0 : go 1 fl0 fr0
  where
    fl0 = M.singleton c (DD 0 N)
    fr0 = (ns,ws,es,ss)
    ns | (C (y-1) x) `S.member` cS_ = [ (C (y-1) x) ]
    ws | (C y (x-1)) `S.member` cS_ = [ (C y (x-1)) ]
    es | (C y (x+1)) `S.member` cS_ = [ (C y (x+1)) ]
    ss | (C (y+1) x) `S.member` cS_ = [ (C (y+1) x) ]
    go i f r = f' : go (i+1) f' r'
      where
        (f',r') = flood1 i cave f r

-- | One iteration of the flooding algorithm: 4 parallel scans for each dir
flood1 :: Int -> Cave -> Flood -> Frontiers -> (Flood,Frontiers)
flood1 i cave@Cave{..} f (n,w,e,s)
  = ( f' , (n',w',e',s') )
  where
    (fn,n') = neighbours cave (M.keysSet f) n
    (fw,w') = neighbours cave fn w
    (fe,e') = neighbours cave fw e
    (fs,s') = neighbours cave fe s
    f' = M.unions [ M.fromList [ (c,DD i N) | c <- n ]
                  , M.fromList [ (c,DD i W) | c <- w ]
                  , M.fromList [ (c,DD i E) | c <- e ]
                  , M.fromList [ (c,DD i S) | c <- s ]
                  , f ]

-- | Neighbouring algorithm
-- it is applied once for each of the four parallel scans (N, W, E, S)
neighbours :: Cave -> S.Set Coord -> [Coord] -> (S.Set Coord,[Coord])
neighbours Cave{..} seen cs = (seen',ns)
  where
    (seen',ns) = L.foldl' collect (seen,[]) (concatMap adjs cs)
      where
        collect (seen,ns) c
          |  c `S.member` cS_
          && c `S.notMember` seen
          && c `M.notMember` cU_ = (S.insert c seen, (c:ns))
          | otherwise                                = (seen           , ns    )

-- | Debug print of a cave flooded at (`y`,`x`) for `n` steps
p y x n = do
  cave <- fromString <$> readFile "input.txt"
  mapM_ (putStrLn . showFlood cave) . take 1 . drop n . flood cave $ C y x

showFlood :: Cave -> Flood -> String
showFlood cave@Cave{..} f = L.intercalate "\n"
  [ concat [ showTile1 cave f (C y x) | x <- [0..cW_-1] ]
    ++ " " ++
    concat [ showTile2 cave f (C y x) | x <- [0..cW_-1] ]
  | y <- [0..cH_-1] ]

showTile1 :: Cave -> Flood -> Coord -> String
showTile1 Cave{..} f c | Just (DD dist dir) <- f M.!? c, dist > 0 =
    --head . show $ dist
    case dir of
      N -> "↑"
      W -> "‹" -- '←'
      E -> "›" -- '→'
      S -> "↓"
showTile1 cave f c = showTile cave c

showTile2 Cave{..} f c | Just (DD dist _) <- f M.!? c, dist > 0 = take 1 . show $ dist
showTile2 cave f c = showTile cave c
