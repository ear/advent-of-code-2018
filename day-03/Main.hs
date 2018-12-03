{-# language TypeApplications #-}
module Main where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main :: IO ()
main = do
  input <- readFile "input.txt"
  let byID = parse input
  let fabric = draw byID
  print . Map.size . Map.filter (>1) $ fabric

-- String -> Map ID ( (x,y) , (w,h) )
parse = toMap . map words . lines . map clean
  where
    clean x | x `elem` "#@,:x" = ' '
    clean x = x

    toMap = Map.fromList . map (fromRect . map (read @Int))
      where
        fromRect [i,x,y,w,h] = (i,((x,y),(w,h)))

-- Map ID ((x,y),(w,h)) -> Map (x,y) ???
draw m = draw' m
  where
    (maxX,maxY) = foldl collect (0,0) m
    collect (mx,my) ((x,y),(w,h)) = (max mx (x+w),max my (y+h))

    coords = [(x,y) | x <- [0..maxX-1], y <- [0..maxY-1]]

    rect (x,y) (w,h) = [(x_,y_) | x_ <- [x..x+w-1], y_ <- [y..y+h-1]]

    draw' = Map.foldl' drawOne Map.empty
    drawOne fabric = foldl' (flip $ Map.alter drawCoord) fabric . uncurry rect
    drawCoord Nothing = Just 1
    drawCoord (Just n) = Just (n+1)

