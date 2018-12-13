module Main where

import Data.Ord
import Data.Foldable

import qualified Data.List as L

import Data.Array.IArray ( Array, (!) )
import qualified Data.Array.IArray as A

type Tile      = Char                          -- - \ | / +
type Direction = Char                          -- ^ > v <
data Decision  = GoLeft | GoStraight | GoRight
type Coord     = (Int,Int)
type Cart      = (Coord,(Direction,Decision))  -- current direction, next decision
type Carts     = Array Coord Cart              -- ??
type Tracks    = (Array Coord Tile, Carts)     -- 150x150, carts' coords

--

fromString :: String -> Tracks
fromString xs = (A.array bounds assocs, A.array bounds [])
  where
    assocs = [ ((x,y),c) | (y,ys) <- zip [0..] (lines xs), (x,c) <- zip [0..] ys ]
    bounds = ( (0,0) , fst $ maximumBy (comparing fst) assocs )

showTracks :: Tracks -> String
showTracks (ts,_) = concat [ [ showTile $ ts ! (x,y) | x <- [0..xM] ] ++ "\n" | y <- [0..yM] ]
  where
    (_,(xM,yM)) = A.bounds ts
    showTile c | c `elem` "-\\|/+^>v<" = c
               | otherwise = ' '

--

nextDecision GoLeft     = GoStraight
nextDecision GoStraight = GoRight
nextDecision GoRight    = GoLeft

--

main = do
  input <- readFile "test.txt"
  let tracks = fromString input
  putStr . showTracks $ tracks
