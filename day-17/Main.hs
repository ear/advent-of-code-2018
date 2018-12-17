{-# language ViewPatterns #-}

module Main where

import Text.Printf

import Data.Bifunctor
import Control.Arrow hiding ( first, second )

--

type Coord = (Int,Int)

--

parse :: String -> [[Coord]]
parse = map parseLine . lines
  where
    parseLine = generate . words . map clean
    clean c | c `elem` "0123456789xy" = c
    clean _ = ' '
    generate ["x",(read -> x),"y",(read -> ym),(read -> yM)]
      = [ (y,x) | y <- [ym..yM] ]
    generate ["y",(read -> y),"x",(read -> xm),(read -> xM)]
      = [ (y,x) | x <- [xm..xM] ]

extent :: Int -> [[Coord]] -> (Coord,Coord)
extent n = padX n . bimap minMax minMax . unzip . concat
  where
    minMax = minimum &&& maximum
    padX n = (const 0 *** subtract n) *** (id *** (n+))

frame :: [[Coord]] -> [[Coord]]
frame yxs = translateX (negate xm) yxs
  where
    translateX :: Int -> [[Coord]] -> [[Coord]]
    translateX dx = map (map (second (dx +)))
    ((ym,yM),(xm,xM)) = extent 1 yxs

--

main = do
  tclay <- frame . parse <$> readFile "test.txt"
  mapM_ print tclay

  let ((ym,yM),(xm,xM)) = extent 0 tclay
  printf "clay veins from (%d,%d) to (%d,%d)\n" ym xm yM xM

  clay <- frame . parse <$> readFile "input.txt"
  let ((ym,yM),(xm,xM)) = extent 0 clay
  printf "clay veins from (%d,%d) to (%d,%d)\n" ym xm yM xM
