{-# language TypeApplications #-}
{-# language RecordWildCards #-}

module Main where

import Debug.Trace

import Data.Int
import Data.Word

import Control.Monad.ST
import qualified Data.Array.ST as ST
import qualified Data.Array.MArray as M
import qualified Data.Array.Unboxed as U
import qualified Data.Array.IArray as I

type T = U.UArray Word32 Int8

part1 :: Int -> [Int]
part1 = takeWhile (>=0) . map fromIntegral . I.elems . make

make :: Int -> T
make n = ST.runSTUArray $
  do arr <- ST.newArray (0,fromIntegral n+12) (-1)
     size <- begin arr
     _ <- tick arr size n
     return arr

begin a = do
  M.writeArray a 0 3
  M.writeArray a 1 7
  return 2

tick a size n = go n size 0 1
  where
    go 0 size _  _  = return size
    go n size e1 e2 = do
      d1 <- M.readArray a e1
      d2 <- M.readArray a e2
      let (x,y) = (d1+d2) `divMod` 10
      let size' | x > 0     = size + 2
                | otherwise = size + 1
      let e1' = (e1 + fromIntegral d1 + 1) `mod` size'
          e2' = (e2 + fromIntegral d2 + 1) `mod` size'
      dump <- takeWhile (>=0) <$> M.getElems a
      if x > 0
      then do M.writeArray a size x
              M.writeArray a (size+1) y
              traceShow dump $ go (pred n) size' e1' e2'
      else do M.writeArray a size y
              traceShow dump $ go (pred n) size' e1' e2'

input = 846601

main = do
  print $ part1 16 -- print the example
