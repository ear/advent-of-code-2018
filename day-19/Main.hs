{-# language TypeApplications #-}
{-# language RecordWildCards #-}

import Debug.Trace

import Machine

import Data.Int

import qualified Data.List as L
import qualified Data.Array.IArray as A

type Table a = A.Array a (M a -> M a)

es :: N a => Table a
es = A.array (0,6) $ zip [0..] $
  [ seti 5 0 1, seti 6 0 2, addi 0 1 0, addr 1 2 3, setr 1 0 0, seti 8 0 4
  , seti 9 0 5 ]

prog :: N a => Table a
prog = A.array (0,35) $ zip [0..] $
  [ addi 3 16 3 , seti 1 3 1  , seti 1 2 4 , mulr 1 4 5 , eqrr 5 2 5
  , addr 5 3 3  , addi 3 1 3  , addr 1 0 0 , addi 4 1 4 , gtrr 4 2 5
  , addr 3 5 3  , seti 2 6 3  , addi 1 1 1 , gtrr 1 2 5 , addr 5 3 3
  , seti 1 0 3  , mulr 3 3 3  , addi 2 2 2 , mulr 2 2 2 , mulr 3 2 2
  , muli 2 11 2 , addi 5 8 5  , mulr 5 3 5 , addi 5 6 5 , addr 2 5 2
  , addr 3 0 3  , seti 0 5 3  , setr 3 0 5 , mulr 5 3 5 , addr 3 5 5
  , mulr 3 5 5  , muli 5 14 5 , mulr 5 3 5 , addr 2 5 2 , seti 0 8 0
  , seti 0 9 3 ]

run :: N a => a -> Table a -> M a
run i t = go emptyMachine
  where
    go m@M{..} | let (ipm,ipM) = A.bounds t in ip < ipm || ip > ipM = m
    go m@M{..} = go m'''
      where
        f = t A.! ip
        m'   = set m i ip
        m''  = f m'
        m''' = set m'' (-1) $ succ $ get m'' i

--main = print (run @Int64 0 es)
main = print (run @Int64 3 prog)
