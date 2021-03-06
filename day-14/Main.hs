{-# language FlexibleContexts #-}
{-# language Rank2Types #-}

module Main where

import Debug.Trace

import Data.Int
import Data.Word
import qualified Data.List as L

import Control.Monad.ST
import Control.Monad.ST.Unsafe
import qualified Data.Array.ST as ST
import qualified Data.Array.MArray as M
import qualified Data.Array.Unboxed as U
import qualified Data.Array.IArray as I

-- https://www.vex.net/~trebla/haskell/annote-STArray.xhtml
-- type STUA s = (Word32,Word32) -> Int8 -> ST s (ST.STUArray s Word32 Int8)
type STUA s = (Int,Int) -> Int-> ST s (ST.STUArray s Int Int)

type Predicate = forall s. Int -> ST.STUArray s Int Int -> ST s Bool
type Extract a = forall s. Int -> ST.STUArray s Int Int -> ST s a

--part1 n = search predicate extract (n+10)
--  where
--    predicate l _ = return $ l > n+10
--    extract _ = concat . map show . take 10 . drop n

part2 ds = search predicate extract (2^25)
  where
    -- n = fst $ L.foldl' (\(s,i) d -> (s+(d*10^i),succ i)) (0,0) ds
    s = length ds
    predicate l _ | l < s+1 = pure False
    predicate l a = do
      xs <- M.readArray a `traverse` [l-s..l-1]
      if xs == ds
      then return True
      else do ys <- M.readArray a `traverse` [l-s-1..l-2]
              return $ ys == ds
    extract l a = do
      xs <- M.readArray a `traverse` [l-s..l-1]
      if ds == xs
      then return $ l-s
      else return $ l-s-1

search :: Predicate -- ^ predicate
       -> Extract a -- ^ compute answer
       -> Int       -- ^ maximum iterations
       -> Maybe a   -- ^ answer
search p e n = runST $
  do a <- (ST.newArray :: STUA s) (0,fromIntegral n + 12) (-1)
     s <- begin a
     go (ceiling $ fromIntegral n / 2) a s 0 1
  where
    go 0 a _    _  _  = return Nothing
    go n a size e1 e2 = do
      d1 <- M.readArray a e1
      d2 <- M.readArray a e2
      let (x,y) = (d1+d2) `divMod` 10
      let size' | x > 0     = size + 2
                | otherwise = size + 1
      let e1' = (e1 + fromIntegral d1 + 1) `mod` size'
          e2' = (e2 + fromIntegral d2 + 1) `mod` size'
      if x > 0
        then do M.writeArray a size     x
                M.writeArray a (size+1) y
        else do M.writeArray a size     y
      done <- p (fromIntegral size') a
      if done
        then Just <$> e (fromIntegral size') a
        else go (pred n) a size' e1' e2'

begin a = do
  M.writeArray a 0 3
  M.writeArray a 1 7
  return 2

input = 846601

main = do
  --print $ part1 input
  --print $ part2 [0,1,2,4,5]
  --print $ part2 [5,1,5,8,9]
  --print $ part2 [9,2,5,1,0]
  --print $ part2 [5,9,4,1,4]
  print $ part2 [8,4,6,6,0,1]
  --print $ part2 [0,8,4,6,0,1]
