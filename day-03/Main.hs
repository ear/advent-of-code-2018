{-# language TypeApplications #-}
{-# language ViewPatterns #-}
module Main where

import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

main :: IO ()
main = do
  input <- readFile "input.txt"
  let byID = parse input
  let fabric = draw byID
  print . Map.size . Map.filter ((>1) . length) $ fabric
  print $ intact byID fabric

-- String -> Map ID ((x,y),(w,h))
parse = toMap . map words . lines . map clean
  where
    clean x | x `elem` "#@,:x" = ' '
    clean x = x

    toMap = Map.fromList . map (fromRect . map (read @Int))
      where
        fromRect [i,x,y,w,h] = (i,((x,y),(w,h)))

rect ((x,y),(w,h)) = [(x_,y_) | x_ <- [x..x+w-1], y_ <- [y..y+h-1]]

-- Map ID ((x,y),(w,h)) -> Map (x,y) [ID]
draw m = fabric
  where
    (maxX,maxY) = foldl collect (0,0) m
    collect (mx,my) ((x,y),(w,h)) = (max mx (x+w),max my (y+h))

    coords = [(x,y) | x <- [0..maxX-1], y <- [0..maxY-1]]

    draw' = Map.foldlWithKey' drawClaim Map.empty

    -- f = fabric; c = claim id; r = claim rectangle (x,y)s
    drawClaim f c (rect -> r) = foldl' (flip $ Map.alter $ drawCoord c) f r

    drawCoord c Nothing   = Just [c]
    drawCoord c (Just cs) = Just (c:cs)

    fabric = draw' m

intact m fabric = intactIDs
  where
    -- ID -> ((x,y),(w,h)) -> Bool
    isIntact c (Set.fromList . rect -> r) =
      Map.null . Map.filter hasOverlap . Map.restrictKeys fabric $ r

    hasOverlap = not . null . tail

    intactIDs = Map.keys $ Map.filterWithKey isIntact m
