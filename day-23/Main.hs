{-# language ViewPatterns #-}
{-# language RecordWildCards #-}
{-# language FlexibleContexts #-}

import Debug.Trace

import Data.Ord
import Data.Foldable
import qualified Data.List as L
import qualified Data.Array.IArray as A
import qualified Data.Array.Unboxed as A

type Point = (Int,Int,Int)

data Bot = Bot { p_ :: Point, r_ :: Int } deriving (Show)

type Overlaps = A.UArray (Int,Int) Bool

-- part 2

part2 bots =
  --minimum [ norm p | p <- [ p_ bot ]
  --                 , sum [ 1 | bot' <- bots, p `inRange` bot' ] == imax ]
  [ sum [ 1 | bot' <- bots, p `inRange` bot' ] | p <- sample ]
  where
    cs = counts bots
    (i,imax) = maximumBy (comparing snd) cs
    bot = bots !! i
    -- these are the bots that need to test the points
    --bots' = map (bots !!) 
    [_,left,_,_,_,_] = vertices bot
    (x,y,z) = left
    sample = (\x -> (x,y,z)) <$> [x, x+100 .. x+1000]
    cubePoints =
      [ (x,y,z) | x <- [xm,xm+step..xM]
                , y <- [ym,ym+step..yM]
                , z <- [zm,zm+step..zM] ]
    ((xm,ym,zm),(xM,yM,zM)) = cube bot
    step = (xM - xm) `div` 200

norm (x,y,z) = abs x + abs y + abs z

cube :: Bot -> (Point,Point)
cube b = ((xm,ym,zm),(xM,yM,zM))
  where
    (xs,ys,zs) = unzip3 $ vertices b
    (xm,xM) = (minimum xs,maximum xs)
    (ym,yM) = (minimum ys,maximum ys)
    (zm,zM) = (minimum zs,maximum zs)

counts :: [Bot] -> [(Int,Int)]
counts = byRows . overlaps
  where
    byRows a =
      [ (i, sum [ 1 | ((i',j),overlap) <- A.assocs a
                    , i == i'
                    , overlap ])
      | i <- [im..iM] ]
      where
        ((im,_),(iM,_)) = A.bounds a


overlaps :: [Bot] -> Overlaps
overlaps bots = A.array ((im,im),(iM,iM)) assocs
  where
    pairs   = zip [0..] bots
    ixs     = map fst pairs
    (im,iM) = (minimum ixs,maximum ixs)
    assocs  = [ ( (i,j), b' `intersects` b || b `intersects` b' )
              | (i,b') <- pairs
              , (j,b ) <- pairs ]

b' `intersects` b = or [ v `inRange` b | v <- vertices b' ]

(x',y',z') `inRange` Bot{..} = abs (x'-x) + abs (y'-y) + abs (z'-z) <= r_
  where (x,y,z) = p_

vertices :: Bot -> [Point]
vertices Bot{..} =
  [ (x+r,y,z), (x-r,y,z)
  , (x,y+r,z), (x,y-r,z)
  , (x,y,z+r), (x,y,z-r) ]
  where
    (x,y,z) = p_
    r = r_

main = do
  bs <- fromString <$> readFile "input.txt"
  --let cs = counts bs
  --mapM_ print . L.sortBy (comparing snd) $ cs
  print $ part2 bs

-- part 1

--inRange b' b = sum (zipWith (\a b -> abs (b - a)) (p_ b) (p_ b')) <= r_ b

--main = do
--  bs <- fromString <$> readFile "input.txt"
--  let b = maximumBy (comparing r_) bs
--  print b
--  print $ length [ b' | b' <- bs, b' `inRange` b ]

-- parsing

fromString :: String -> [Bot]
fromString = map (toBot . words . map clean) . lines
  where clean c | c `elem` "-0123456789" = c | otherwise = ' '
        toBot [read -> x, read -> y, read -> z, read -> r] = Bot (x,y,z) r
