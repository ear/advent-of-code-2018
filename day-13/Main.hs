{-# language LambdaCase #-}
{-# language PatternSynonyms #-}

module Main where

import Data.Ord
import Data.Either
import Data.Foldable
import Data.Bifunctor

import qualified Data.List as L

import Data.Map.Strict ( Map, (!?) )
import qualified Data.Map.Strict as M

import Data.Array.IArray ( Array, (!) )
import qualified Data.Array.IArray as A

--

swap (x,y) = (y,x)

--

type Tile      = Char                          -- - \ | / +
type Direction = Char                          -- ^ > v < X
data Decision  = GoLeft | GoStraight | GoRight deriving Show
type Coord     = (Int,Int)
type Cart      = (Direction,Decision)          -- current direction, next decision
type Carts     = Map Coord Cart                -- (y,x) -> (direction,decision)
type Tracks    = Array Coord Tile              -- 150x150
type System    = (Tracks,Carts)

--

fromString :: String -> System
fromString xs = (patch tracks,carts)
  where
    assocs = [ ((x,y),c) | (y,ys) <- zip [0..] (lines xs), (x,c) <- zip [0..] ys ]
    bounds = ( (0,0) , fst $ maximumBy (comparing fst) assocs )
    tracks = A.array bounds assocs

    carts = M.fromList . map (bimap swap asCart) . filter (isCart . snd) $ assocs
    isCart c = c `elem` "^>v<"

    -- replace carts with tracks
    patch = A.amap (\case t | t `elem` "^v" -> '|' | t `elem` "><" -> '-' | otherwise -> t)

    asCart c = (c,GoLeft) -- augment cart character with a decision

showSystem :: System -> String -- overlays tiles with carts
showSystem (ts,cs) = concat [ [ showTile (x,y) | x <- [0..xM] ] ++ "\n" | y <- [0..yM] ]
                  ++ show cs ++ "\n"
  where
    (_,(xM,yM)) = A.bounds ts
    showTile (x,y)
      = case cs !? (y,x) of
          Just (dir,_) -> dir
          Nothing      -> ts ! (x,y)

--

pattern Crash c s <- (Just c, s)
pattern Happy   s <- (Nothing,s)

-- | Returns either:
--   * Left (first crash coordinate, system froze at the crash)
--   * Right (new system)
tick :: System -> Either (Coord,System) System
tick s@(ts,cs)
  = case L.foldl' step (Nothing,s) (M.toAscList cs) of -- (y,x) means toAscList picks carts in correct order
      Crash c s' -> Left (c,s')
      Happy   s' -> Right s'

-- | (Maybe crash, current system) -> cart -> (Maybe crash, system froze at crash)
step :: (Maybe Coord,System) -> (Coord,Cart) -> (Maybe Coord,System)
step s@(Crash _ _) _ = s
step   (Happy   s) c
  | crashed   = (Just xy',s')
  | otherwise = (Nothing ,s')
  where
    (crashed,xy',s') = cartStep s c

cartStep :: System -> (Coord,Cart) -> (Bool,Coord,System)
cartStep (ts,cs) ((y,x),(dir,dec))
  | dir == 'X' = error $ "tring to move a crashed cart at " ++ show (x,y)
  | otherwise = (crashed,(x',y'),s')
  where
    (x',y') = (x,y) -- TODO not moving
    dir' = 'X' -- TODO just crashing
    s' = (ts,cs) -- TODO not updating cs
    crashed = dir' == 'X'

forward :: Direction -> Coord -> Coord
forward '^' (x,y) = (x,y-3)
forward '>' (x,y) = (x+1,y)
forward 'v' (x,y) = (x,y+1)
forward '<' (x,y) = (x-1,y)

--

-- testing helpers, p input number prints successive steps unsafely
u = \case (Right s) -> s; (Left (_,s)) -> s -- unsafe extract
p i n = mapM_ (putStrLn . showSystem) . take n . map u . iterate (tick . u) . Right . fromString $ i

--

nextDecision GoLeft     = GoStraight
nextDecision GoStraight = GoRight
nextDecision GoRight    = GoLeft

--

part1 :: System -> Coord
part1 = fst . fromLeft (error "??") . until isLeft (tick . fromRight (error "??")) . Right

--

main = do
  input <- readFile "test.txt"
  let s = fromString input
  putStr . showSystem $ s
