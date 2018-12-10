{-# language ViewPatterns #-}

module Main where

import Data.Map.Strict ( Map )

import qualified Data.Map.Strict as M
import qualified Data.List       as L

type P = (Int,Int) -- Position
type V = (Int,Int) -- Velocity

-- Parse

type Entry = (P,V) -- one input entry

parse :: String -> Entry
parse = (\[a,b,c,d] -> ((a,b),(c,d))) . map read . words . map clean
  where clean c | c `elem` "0123456789-" = c | otherwise = ' '

-- Sky

type Sky = Map P V

mkSky :: [Entry] -> Sky
mkSky = M.fromList

showLight :: Sky -> P -> Char
showLight s c | c `M.member` s = '#' | otherwise = '.'

showSky :: Sky -> String
showSky s = L.intercalate "\n" [ [ showLight s (x,y) | x <- rangeX ] | y <- rangeY ]
  where
    (minMax -> (xm,xM), minMax -> (ym,yM)) = unzip . M.keys $ s
    rangeX = [xm .. xM]
    rangeY = [ym .. yM]

minMax :: (Ord a, Foldable f) => f a -> (a,a)
minMax xs = (minimum xs, maximum xs)

p :: Sky -> IO ()
p = putStrLn . showSky

-- Time

tick :: Sky -> Sky
tick = M.mapWithKey move

move :: P -> V -> P
move (x,y) (vx,vy) = (x+vx,x+vy)

-- Main

main = do
  s <- mkSky . map parse . lines <$> readFile "test.txt"
  mapM_ (\s -> p s >> putChar '\n') . iterate tick $ s
