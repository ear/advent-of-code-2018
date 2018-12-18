import Debug.Trace

import Data.Foldable
import Data.Bifunctor

import Control.Arrow hiding ( second )
import Control.Monad.ST

import qualified Data.Array.ST      as A
import qualified Data.Array.Unboxed as A
import qualified Data.Array.MArray  as A
import qualified Data.Array.IArray  as A

type Coord = (Int,Int)

type STUA s = (Coord,Coord) -> [Int] -> ST s (A.STUArray s Coord Int)
type STUA' s = (Coord,Coord) -> Int -> ST s (A.STUArray s Coord Int)

data Acre = Open | Tree | Yard
  deriving (Show, Eq, Ord, Enum)

-- | evolving a (0,0) (yM,xM) grid
evolve :: Coord -> [(Coord,Acre)] -> Int -> A.UArray Coord Int
evolve (h,w) yxs n = A.runSTUArray $ do
  a <- (A.newListArray :: STUA s ) ((0,0),(h-1,w-1)) . map (fromEnum . snd) $ yxs
  b <- (A.newArray     :: STUA' s) ((0,0),(h-1,w-1)) (-1)
  forM_ [0..n] $ \i -> do
    tick a b (i `mod` 2 == 0) -- First time a=a, b=b; then a=b, b=a; etc.
  return b

tick a0 a1 which = do
  x <- A.readArray a (0,0)
  traceShowM x
  A.writeArray b (0,0) x
  where
    -- swap the buffers at each iteration
    a | which = a0 | otherwise = a1
    b | which = a1 | otherwise = a0

parse = (largest &&& flatten) . map (second $ zip [0..] . map fromChar) . zip [0..] . lines
  where
    fromChar '.' = Open
    fromChar '|' = Tree
    fromChar '#' = Yard
    largest yxs = (length yxs, length . snd . head $ yxs)
    flatten yxs = [ ((y,x),c) | (y,xs) <- yxs, (x,c) <- xs ]

main :: IO ()
main = do
  (size,coords) <- parse <$> readFile "test.txt"
  print coords
  print size
  print $ evolve size coords 1
