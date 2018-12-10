{-# language ViewPatterns #-}

module Main where

-- import Debug.Trace

import Data.Map.Strict ( Map )

import qualified Data.Map.Strict as M
import qualified Data.List       as L

traceShow _ x = x
traceShowId x = x

type P = (Int,Int) -- Position
type V = (Int,Int) -- Velocity

-- Parse

type Entry = (P,V) -- one input entry

parse :: String -> Entry
parse = (\[a,b,c,d] -> ((a,b),(c,d))) . map read . words . map clean
  where clean c | c `elem` "0123456789-" = c | otherwise = ' '

-- Sky

type Sky = [(P,V)]

mkSky :: [Entry] -> Sky
mkSky = id

showLight :: Sky -> P -> Char
showLight s c | c `elem` (map fst s) = '#' | otherwise = '.'

showSky :: Sky -> String
showSky s = L.intercalate "\n" [ [ showLight s (x,y) | x <- rangeX ] | y <- rangeY ]
  where
    (minMax -> (xm,xM), minMax -> (ym,yM)) = unzip $ map fst s
    rangeX = traceShowId [xm .. xM]
    rangeY = traceShowId [ym .. yM]

minMax :: (Ord a, Foldable f) => f a -> (a,a)
minMax xs = (minimum xs, maximum xs)

area :: Sky -> Int
area s = (xM - xm) * (yM - ym)
  where
    (minMax -> (xm,xM), minMax -> (ym,yM)) = unzip $ map fst s

-- Time

tick :: Sky -> Sky
tick = map (\(p,v) -> (move p v,v))

move :: P -> V -> P
move (x,y) (vx,vy) = (x+vx,y+vy)

-- Movie

p :: Sky -> IO ()
p = putStrLn . showSky

movie :: Sky -> Int -> IO ()
movie s 0 = p s
movie s n = traceShow s $ p s >> movie (tick s) (pred n)

movie_ s 0 = p s
movie_ s n = movie_ (tick s) (pred n)

-- Main

main = do
  s <- mkSky . map parse . lines <$> readFile "input.txt"
  -- mapM_ (\s -> p s >> putChar '\n') . take 4 . iterate tick $ s
  let ss = iterate tick s
  let iass = map (\(i,sky) -> (area sky,(i,sky))) . zip [0..] $ ss
  let i = findMin iass
  movie (ss !! (i-2)) 5
  print i

findMin :: [(Int,(Int,Sky))] -> Int
findMin = go maxBound
  where
    go m ((a,(i,_)):xs) | a > m = i-1 | otherwise = go a xs
