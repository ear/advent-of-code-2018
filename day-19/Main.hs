{-# language TypeApplications #-}
{-# language RecordWildCards #-}
{-# language ViewPatterns #-}
{-# language FlexibleContexts #-}

import Text.Printf
import Debug.Trace

import Machine

import Data.Int

import qualified Data.List as L
import qualified Data.Map.Strict as M
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

syms :: A.Array Int64 String
syms = A.array (0,35) $ zip [0..] $
  [ "r3 = r3 + 16;   "
  , "r1 = 1;         "
  , "r4 = 1;         "
  , "r5 = r1 * r4;   "
  , "r5 = (r5 == r2);"
  , "jmp +r5;     (*)" -- "r3 = r5 + r3;   "
  , "jmp +1;         " -- "r3 = r3 + 1;    "
  , "r0 = r1 + r0;   "
  , "r4 = r4 + 1;    "
  , "r5 = (r4 > r2); "
  , "jmp +r5;    (**)" -- "r3 = r3 + r5;   "
  , "jmp @2;         " -- "r3 = 2;         "
  , "r1 = r1 + 1;    "
  , "r5 = (r1 > r2); "
  , "jmp +r5;   (***)" -- " -- "r3 = r5 + r3;   "
  , "jmp @1;         " -- "r3 = 1;         "
  , "jmp +(r3*r3);   " -- "r3 = r3 * r3;   "
  , "r2 = r2 + 2;    "
  , "r2 = r2 * r2;   "
  , "r2 = r3 * r2;   "
  , "r2 = r2 * 11;   "
  , "r5 = r5 + 8;    "
  , "r5 = r5 * r3;   "
  , "r5 = r5 + 6;    "
  , "r2 = r2 + r5;   "
  , "jmp +r0;        " -- "r3 = r3 + r0;   "
  , "jmp @0;         " -- "r3 = 0;         "
  , "r5 = r3;        "
  , "r5 = r5 * r3;   "
  , "r5 = r3 + r5;   "
  , "r5 = r3 * r5;   "
  , "r5 = r5 * 14;   "
  , "r5 = r5 * r3;   "
  , "r2 = r2 + r5;   "
  , "r0 = 0;         "
  , "jmp @0;         " ] -- "r3 = 0;         " ]

-- debugging structure
data D a = D
  { iter_ :: Int             -- current Iteration
  , ip_   :: a               -- current IP
  , m_    :: M a             -- current machine state
  , ips_  :: M.Map a Int     -- histogram of past IPs
  , injs_ :: M.Map Int (M a) -- inject Machine at particular Iteration
  }
incrIter d@D{..} = d { iter_ = succ iter_ }
collectIP ip d@D{..} = d
  { ips_ = M.insertWith (+) ip 1 ips_
  , ip_  = ip }
type P = String -> Int -> Int64 -> String -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> Int64 -> String -> String
dump D{..}
  = (printf :: P) "%4d: %3d  %s  [ip:%3d r0:%d r1:%d r2:%d r3:%d r4:%d r5:%d]  %s"
    iter_ ip_ (syms A.! ip_) (ip m_) (r0 m_) (r1 m_) (r2 m_) (r3 m_) (r4 m_) (r5 m_) (dumpIPs ips_)
dumpIPs
  = L.intercalate " " . map (uncurry $ printf "(%d)%d")
  . filter (\(ip,_) -> ip < 18) . M.toAscList

-- next idea: inject code at a specific iteration #
-- in place of / before / after ?
-- even better yet: set the M a to the desired state and that's it.
-- this one should  not change the output
injs = M.fromList $
  -- [(22, M 5 0 1 10551418 4 1 0)] -- orig
  -- [(22, M 5 0 1 10551418 4 1 10551418 )] -- replace
  [(26, M 9 0 1 10551418 8 10551419 0)]

--run :: N a => a -> Table a -> M a
run :: Int64 -> Table Int64 -> M Int64
run i t = go (D 0 0 emptyMachine M.empty injs) emptyMachine
  where
    go _ m@M{..} | let (ipm,ipM) = A.bounds t in ip < ipm || ip > ipM = m
    go d@D{..} m@M{..} = go (trace (dump d') d') m'''
      where
        f = t A.! ip
        m'   = set m i ip
        m''  = f m'
             -- debugger injection
        m''' | Just im <- injs_ M.!? succ iter_ = trace (printf "INJECTION @ %d" (succ iter_)) im
             -- regular evaluation
             | otherwise = set m'' (-1) $ succ $ get m'' i
        -- debugger
        d' = incrIter $ collectIP ip $ d { m_ = m''' }

--main = print (run @Int64 0 es)
main = print (run 3 prog)
