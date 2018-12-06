{-# language ViewPatterns #-}
module Main where

import qualified Data.List as L

import Data.Map (Map, (!))
import qualified Data.Map as M

import Data.Foldable (for_)

import Control.Applicative (liftA2)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input.txt"
  let e = extend . extent $ input
  --print e
  --print (topleft e, bottomright e)
  let g = draw e input
  --printGrid g
  let as' = areas g
  --M.traverseWithKey (\k v -> print (k,v)) as'
  --print . maxArea $ as'
  let r = region 10000 input g
  print $ length r
  printRegion r


-- Parsing

parse :: String -> Coord
parse = (\[a,b]->(a,b)) . map read . words . map clean
  where
    clean ',' = ' '
    clean  c  =  c


-- Coordinates

-- ( x , y )
type Coord = (Int,Int)

closest :: [Coord] -> Coord -> Maybe Int
closest cs c = ans
  where
    distances = map (manhattan c) cs
    d = minimum distances
    is = L.findIndices (d ==) distances
    ans | [i] <- is = Just i
        | otherwise = Nothing

manhattan :: Coord -> Coord -> Int
manhattan (x0,y0) (x1,y1) = abs (x1-x0) + abs (y1-y0)


-- Extents

-- ( (x min, x max), (y min, y max) )
type Extent = ((Int,Int),(Int,Int))

extent :: [Coord] -> Extent
extent cs = ((xm,xM),(ym,yM))
  where
    (L.sort -> xs,L.sort -> ys) = unzip cs
    (xm,xM) = (head xs,last xs)
    (ym,yM) = (head ys,last ys)

extend :: Extent -> Extent
extend ((xm,xM),(ym,yM)) = ((xm-ex,xM+ex),(ym-ey,yM+ey))
  where
    w = xM - xm + 1
    h = yM - ym + 1
    ex = ceiling (fromIntegral w / 3)
    ey = ceiling (fromIntegral h / 3)

topleft, bottomright :: Extent -> Coord
topleft     ((x,_),(y,_)) = (x,y)
bottomright ((_,x),(_,y)) = (x,y)

rows :: Extent -> [[Coord]]
rows ((xm,xM),(ym,yM)) = [ [ (x,y) | x <- [xm..xM] ] | y <- [ym..yM] ]

coords :: Extent -> [Coord]
coords = concat . rows


-- Grid

-- Int = coordinates' index in the input
type Grid = (Extent, Map Coord (Maybe Int))

draw :: Extent -> [Coord] -> Grid
draw e cs = (e,grid)
  where
    xys = coords e
    grid = M.fromAscList [(c,closest cs c) | c <- xys]

onEdge :: Coord -> Grid -> Bool
(x,y) `onEdge` ( ((xm,xM),(ym,yM)), _ )
    = x == xm || x == xM || y == ym || y == yM



-- Drawing a pretty grid

printGrid :: Grid -> IO ()
printGrid g@(e,_) = do
  for_ xys $ \ys -> do
    for_ ys $ \(x,y) -> do
      putChar (gridChar g (x,y))
    putChar '\n'
  where
    xys = rows e

gridChar :: Grid -> Coord -> Char
gridChar (_,m) c
  = case M.lookup c m of
      Nothing       -> '.'
      Just Nothing  -> '.'
      Just (Just i) -> toEnum . (i+) . fromEnum $ 'A'


-- Areas

-- Nothing = infinite area
-- Just n  = area of size n
type Areas = Map Int (Maybe Int)

areas :: Grid -> Areas
areas g@(e,gm) = ans
  where
    ans = L.foldl' walk M.empty (coords e)

    walk m c | Just _ <- mi = count
             | otherwise    = m
      where
        mi@(~(Just i)) = gm ! c
        count | c `onEdge` g = M.insert i Nothing m
              | otherwise    = M.insertWith (liftA2 (+)) i (Just 1) m

maxArea :: Areas -> Maybe Int
maxArea = maximum


-- Region

region :: Int -> [Coord] -> Grid -> [Coord]
region n cs = filter inside . coords . fst
  where
    inside c = sum [manhattan c c' | c' <- cs] < n


-- Draw a pretty region

printRegion :: [Coord] -> IO ()
printRegion cs = do
  for_ xys $ \ys -> do
    for_ ys $ \c -> do
      putChar (regionChar c)
    putChar '\n'
  where
    xys = rows $ extent cs
    regionChar c | c `elem` cs = 'O'
                 | otherwise   = '.'
