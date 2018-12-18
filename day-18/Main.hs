import Data.Bifunctor

import Control.Arrow hiding ( second )
import Control.Monad.ST

import qualified Data.Array.ST      as A
import qualified Data.Array.Unboxed as A
import qualified Data.Array.MArray  as A
import qualified Data.Array.IArray  as A

type Coord = (Int,Int)

type STUA s = (Coord,Coord) -> Int -> ST s (A.STUArray s Coord Int)

data Acre = Open | Tree | Yard
  deriving (Show, Eq, Ord, Enum)

-- | evolving a (0,0) (yM,xM) grid
evolve :: Coord -> [(Coord,Acre)] -> Int -> A.UArray Coord Int
evolve (yM,xM) yxs n = A.runSTUArray $ do
  a <- (A.newArray :: STUA s) ((0,0),(yM,xM)) (-1)
  return a

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
  print $ evolve size coords 0
