{-# language PatternSynonyms #-}

module Main where

import Data.Ord
import Data.Either
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
type Direction = Char                          -- ^ > v < X
data Decision  = GoLeft | GoStraight | GoRight deriving Show
type Coord     = (Int,Int)
type Cart      = (Direction,Decision)          -- current direction, next decision
type Carts     = Map Coord Cart                -- (y,x) -> (direction,decision)
type Tracks    = Array Coord Tile              -- 150x150
type System    = (Tracks,Carts)

--

fromString :: String -> System
fromString xs = (tracks,carts)
  where
    assocs = [ ((x,y),c) | (y,ys) <- zip [0..] (lines xs), (x,c) <- zip [0..] ys ]
    bounds = ( (0,0) , fst $ maximumBy (comparing fst) assocs )
    tracks = A.array bounds assocs
    isCart c = c `elem` "^>v<"
    carts = M.fromList . map (bimap swap fromChar) . filter (isCart . snd) $ assocs

fromChar :: Char -> Cart
fromChar c = (c,GoLeft)

showSystem :: System -> String
showSystem (ts,cs) = concat [ [ showTile $ ts ! (x,y) | x <- [0..xM] ] ++ "\n" | y <- [0..yM] ]
                  ++ show cs ++ "\n"
  where
    (_,(xM,yM)) = A.bounds ts
    showTile c | c `elem` "-\\|/+^>v<" = c
               | otherwise = ' '

--

pattern Crash c s <- (Just c, s)
pattern Happy   s <- (Nothing,s)

-- | Returns either:
--   * Left (first crash coordinate, system froze at the crash)
--   * Right (new system)
tick :: System -> Either (Coord,System) System
tick s@(ts,cs)
  = case L.foldl' step (Nothing,s) (M.toAscList cs) of
      Crash c s' -> Left (c,s')
      Happy   s' -> Right s'

-- | (Maybe crash, current system) -> cart -> (Maybe crash, system froze at crash)
step :: (Maybe Coord,System) -> (Coord,Cart) -> (Maybe Coord,System)
step s@(Crash _ _)   _ = s
step   (Happy s) c
  = case dir' of
      'X' -> (Just (x',y'),s')
  where
    (yx',dir',s') = cartStep s c
    (y',x') = yx'

cartStep :: System -> (Coord,Cart) -> (Coord,Direction,System)
cartStep (ts,cs) ((y,x),(dir,dec)) = ((y',x'),dir',s')
  where
    (y',x') = (y,x)
    dir' = 'X' -- TODO just crashing
    s' = (ts,cs) -- TODO not updating ts nor cs

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
