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



part2 _ = [()]

walk bots p@(x,y,z) = minimumBy (comparing norm) [a,b,c]
  where
    a_n = last $ takeWhile (\n -> 891 <= intersection bots (x-3*n,y    ,z-3*n)) [0..]
    a   = diff p (3*a_n,0    ,3*a_n)
    b_n = last $ takeWhile (\n -> 891 <= intersection bots (x-3*n,y-3*n,z    )) [0..]
    b   = diff p (3*b_n,3*b_n,0    )
    c_n = last $ takeWhile (\n -> 891 <= intersection bots (x    ,y-3*n,z-3*n)) [0..]
    c   = diff p (0    ,3*c_n,3*c_n)

diff :: Point -> Point -> Point
diff (x0,y0,z0) (x1,y1,z1) = (x0-x1,y0-y1,z0-z1)

add :: Point -> Point -> Point
add (x0,y0,z0) (x1,y1,z1) = (x0+x1,y0+y1,z0+z1)

exhaust' bots points = maximumBy (comparing (intersection bots) <> comparing (\p -> -1 * (norm p))) points

exhaust bots p r = best
  where ((xm,ym,zm),(xM,yM,zM)) = cube (Bot p r)
        cubePoints = [ (x,y,z) | x<-[xm..xM], y<-[ym..yM], z<-[zm..zM] ]
        best = maximumBy (comparing (intersection bots) <> comparing (\v -> -1 * (norm v))) cubePoints

intersection bots p = length [ undefined | b <- bots, p `inRange` b ]

dist (x0,y0,z0) (x1,y1,z1) = abs (x0-x1) + abs (y0-y1) + abs (z0-z1)

oct :: Point -> Int -> [Point]
oct = (vertices .) . Bot

cubeVerts :: Point -> Int -> [Point]
cubeVerts p r = [ (x,y,z) | x <- [xm,xM], y <- [ym,yM], z <- [zm,zM] ]
  where
    ((xm,ym,zm),(xM,yM,zM))= cube (Bot p r)

norm (x,y,z) = abs x + abs y + abs z

cube :: Bot -> (Point,Point)
cube b = ((xm,ym,zm),(xM,yM,zM))
  where
    (xs,ys,zs) = unzip3 $ vertices b
    (xm,xM) = (minimum xs,maximum xs)
    (ym,yM) = (minimum ys,maximum ys)
    (zm,zM) = (minimum zs,maximum zs)

counts :: [Bot] -> [(Int,[Bot])]
counts bots = byRows $ overlaps bots
  where
    byRows a =
      [ (i, [ bots !! j
            | ((i',j),overlap) <- A.assocs a, i == i', overlap ])
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
vertices Bot{..} = L.nub $
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
  mapM_ print $ part2 bs

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
