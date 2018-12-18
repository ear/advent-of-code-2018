{-# language PatternSynonyms #-}
{-# language FlexibleContexts #-}
import Debug.Trace

import Data.Foldable
import Data.Bifunctor
import Data.Traversable

import Control.Arrow hiding ( second )
import Control.Monad
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

pattern OpenA = 0
pattern TreeA = 1
pattern YardA = 2

resources area = count TreeA area * count YardA area

-- | evolving a (0,0) (yM,xM) grid
evolve :: Coord -> [(Coord,Acre)] -> Int -> A.UArray Coord Int
evolve (h,w) yxs n = A.runSTUArray $ do
  a <- (A.newListArray :: STUA s ) ((0,0),(h-1,w-1)) . map (fromEnum . snd) $ yxs
  b <- (A.newArray     :: STUA' s) ((0,0),(h-1,w-1)) (-1)
  forM_ [1..n] $ \i -> do
    --when (i `mod` 10000 == 0) $ do
    area <- A.getElems (if i `mod` 2 == 0 then b else a)
    traceShowM (i,resources area)
    tick (h,w) a b (i `mod` 2 == 0) -- First time a=a, b=b; then a=b, b=a; etc.
  return $ if (n `mod` 2 == 0) then a else b

check :: Int -> Maybe Int -> Maybe Int
check _ Nothing = Just 1
check j (Just i) = trace ("iteration " ++ show j ++ " repeats iteration " ++ show i) $ Just i

tick (h,w) a0 a1 which = do
  -- clear (h,w) b -- XXX not needed?
  forM_ [0..h-1] $ \y -> do
    forM_ [0..w-1] $ \x -> do
      n <- A.readArray a (y,x)
      ns <- neighbours h w a y x
      --traceShowM ns
      A.writeArray b (y,x) (next n ns)
  where
    -- swap the buffers at each iteration
    a | which = a1 | otherwise = a0
    b | which = a0 | otherwise = a1

next :: Int -> [Int] -> Int
next OpenA ns | count TreeA ns >= 3 = TreeA
              | otherwise           = OpenA
next TreeA ns | count YardA ns >= 3 = YardA
              | otherwise           = TreeA
next YardA ns | any (YardA ==) ns
             && any (TreeA ==) ns   = YardA
              | otherwise           = OpenA

count :: Eq a => a -> [a] -> Int
count x = length . filter (x ==)

neighbours h w a y0 x0 = concat . map concat <$> do
  forM [max 0 (y0-1) .. min (h-1) (y0+1)] $ \y -> do
    forM [max 0 (x0-1) .. min (w-1) (x0+1)] $ \x -> do
      if (y,x) == (y0,x0)
      then return []
      else do
        n <- A.readArray a (y,x)
        pure [n]

clear (h,w) a = do
  forM_ [0..h-1] $ \y -> do
    forM_ [0..w-1] $ \x -> do
      A.writeArray a (y,x) (-1)

parse = (largest &&& flatten) . map (second $ zip [0..] . map fromChar) . zip [0..] . lines
  where
    fromChar '.' = Open
    fromChar '|' = Tree
    fromChar '#' = Yard
    largest yxs = (length yxs, length . snd . head $ yxs)
    flatten yxs = [ ((y,x),c) | (y,xs) <- yxs, (x,c) <- xs ]

main :: IO ()
main = do
  (size,coords) <- parse <$> readFile "input.txt"
  print size
  let area = evolve size coords 1000000000
  --print area
  let trees = count TreeA $ A.elems area
      yards = count YardA $ A.elems area
  print (trees,yards,trees*yards)
