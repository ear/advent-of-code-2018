{-# language ViewPatterns #-}
module Main where

import qualified Data.List as L
import Data.Map (Map)
import qualified Data.Map as M
import Data.Foldable (for_)

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input.txt"
  let e = extend . extent $ input
  print e
  print (topleft e, bottomright e)
  let g = draw e input
  printGrid g


-- ( x , y )
type Coord = (Int,Int)

parse :: String -> Coord
parse = (\[a,b]->(a,b)) . map read . words . map clean
  where
    clean ',' = ' '
    clean  c  =  c


-- ( (x min, x max), (y min, y max) )
type Extent = ((Int,Int),(Int,Int))

extent :: [(Int,Int)] -> Extent
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

coords :: Extent -> [Coord]
coords e = [(x,y) | x <- xs, y <- ys]
  where
    (xm,ym) = topleft e
    (xM,yM) = bottomright e

    xs = [xm..xM]
    ys = [ym..yM]


-- Int = coordinates' index in the input
type Grid = (Extent, Map Coord (Maybe Int))

draw :: Extent -> [Coord] -> Grid
draw e cs = (e,grid)
  where
    xys = coords e
    grid = M.fromAscList [(c,closest cs c) | c <- xys]

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


-- Drawing a pretty picture

gridExtent :: Grid -> Extent
gridExtent (e,_) = e

printGrid :: Grid -> IO ()
printGrid g = do
  for_ ys $ \y -> do
    for_ xs $ \x -> do
      putChar (gridChar g (x,y))
    putChar '\n'
  where
    xys = coords (gridExtent g)
    xs = L.nub $ L.sort $ map fst xys
    ys = L.nub $ L.sort $ map snd xys

gridChar :: Grid -> Coord -> Char
gridChar (_,m) c
  = case M.lookup c m of
      Nothing       -> '.'
      Just Nothing  -> '.'
      Just (Just i) -> toEnum . (i+) . fromEnum $ 'A'
