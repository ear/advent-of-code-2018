module Main where

import Data.Ord
import Data.Foldable
import Data.Bifunctor

import qualified Data.List as L

import Data.Map.Strict ( Map )
import qualified Data.Map.Strict as M

import Data.Array.IArray ( Array, (!) )
import qualified Data.Array.IArray as A

--

swap (x,y) = (y,x)

--

type Tile      = Char                          -- - \ | / +
type Direction = Char                          -- ^ > v <
data Decision  = GoLeft | GoStraight | GoRight deriving Show
type Coord     = (Int,Int)
type Cart      = (Direction,Decision)          -- current direction, next decision
type Carts     = Map Coord Cart                -- (y,x) -> (direction,decision)
type Tracks    = (Array Coord Tile, Carts)     -- 150x150, carts' coords

--

fromString :: String -> Tracks
fromString xs = (tracks,carts)
  where
    assocs = [ ((x,y),c) | (y,ys) <- zip [0..] (lines xs), (x,c) <- zip [0..] ys ]
    bounds = ( (0,0) , fst $ maximumBy (comparing fst) assocs )
    tracks = A.array bounds assocs
    isCart c = c `elem` "^>v<"
    carts = M.fromList . map (bimap swap fromChar) . filter (isCart . snd) $ assocs

fromChar :: Char -> Cart
fromChar c = (c,GoLeft)

showTracks :: Tracks -> String
showTracks (ts,cs) = concat [ [ showTile $ ts ! (x,y) | x <- [0..xM] ] ++ "\n" | y <- [0..yM] ]
                  ++ show cs ++ "\n"
  where
    (_,(xM,yM)) = A.bounds ts
    showTile c | c `elem` "-\\|/+^>v<" = c
               | otherwise = ' '

--

-- | Returns either: Left (crash,tracks) or Right tracks
tick :: Tracks -> Either (Coord,Tracks) Tracks
tick ts = Right ts

--

nextDecision GoLeft     = GoStraight
nextDecision GoStraight = GoRight
nextDecision GoRight    = GoLeft

--

main = do
  input <- readFile "test.txt"
  let tracks = fromString input
  putStr . showTracks $ tracks
