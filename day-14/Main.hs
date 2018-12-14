{-# language FlexibleContexts #-}

module Main where

import Debug.Trace

import Data.Int
import Data.Word

import Control.Monad.ST
import Control.Monad.ST.Unsafe
import qualified Data.Array.ST as ST
import qualified Data.Array.MArray as M
import qualified Data.Array.Unboxed as U
import qualified Data.Array.IArray as I

-- https://www.vex.net/~trebla/haskell/annote-STArray.xhtml
-- type STUA s = (Word32,Word32) -> Int8 -> ST s (ST.STUArray s Word32 Int8)
type STUA s = (Int,Int) -> Int-> ST s (ST.STUArray s Int Int)

--part1 :: Int -> [Int]
--part1 = takeWhile (>=0) . map fromIntegral . I.elems . make
part1 n = search predicate extract (n+10)
  where
    predicate l _ = l > n+10
    extract _ = concat . map show . take 10 . drop n

type Predicate = Int -> [Int] -> Bool
type Extract a = Int -> [Int] -> a

search :: Predicate -- ^ predicate
       -> Extract a -- ^ compute answer
       -> Int       -- ^ maximum iterations
       -> Maybe a   -- ^ answer
search p e n = runST $
  do a <- (ST.newArray :: STUA s) (0,fromIntegral n + 12) (-1)
     s <- begin a
     go n a s 0 1
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
      xs <- unsafeInterleaveST (takeWhile (>=0) <$> M.getElems a)
      if p (fromIntegral size') (map fromIntegral xs)
      then return $ Just $ e (fromIntegral size') (map fromIntegral xs)
      else go (pred n) a size' e1' e2'

begin a = do
  M.writeArray a 0 3
  M.writeArray a 1 7
  return 2

input = 846601

main = do
  print $ part1 input
