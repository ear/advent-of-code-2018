{-# language ConstraintKinds #-}
{-# language RecordWildCards #-}

module Machine where

import Data.Int
import Data.Bits
import qualified Data.List as L
import qualified Data.Array.IArray as A


-- | Machine (four registers: r0,r1,r2,r3)

data M a = M
  { ip :: !a
  , r0 :: !a
  , r1 :: !a
  , r2 :: !a
  , r3 :: !a
  , r4 :: !a
  , r5 :: !a }
  deriving (Show,Eq)


-- | Machine number

type N a = (Integral a, Bits a, Ord a, Read a, Show a, A.Ix a)


-- | Programs

type Instruction a = (a,a,a,a)

type Program a = [Instruction a]


-- | Construction

emptyMachine :: N a => M a
emptyMachine = M 0 0 0 0 0 0 0


-- | Primitives

set :: N a => M a -> a -> a -> M a
set _ i _ | i < -1 || i > 5 = error "setting non-existing register"
set m (-1) x = m { ip = x }
set m 0 x = m { r0 = x }
set m 1 x = m { r1 = x }
set m 2 x = m { r2 = x }
set m 3 x = m { r3 = x }
set m 4 x = m { r4 = x }
set m 5 x = m { r5 = x }

get :: N a => M a -> a -> a
get _ i | i < -1 || i > 5 = error "getting non-existing register"
get M{..} (-1) = ip
get M{..} 0 = r0
get M{..} 1 = r1
get M{..} 2 = r2
get M{..} 3 = r3
get M{..} 4 = r4
get M{..} 5 = r5


-- | Operations

type Op a = a -> a -> a -> M a -> M a

ops :: N a => [(String,Op a)]
ops =
  [ ("addr",addr)
  , ("addi",addi)
  , ("mulr",mulr)
  , ("muli",muli)
  , ("banr",banr)
  , ("bani",bani)
  , ("borr",borr)
  , ("bori",bori)
  , ("setr",setr)
  , ("seti",seti)
  , ("gtir",gtir)
  , ("gtri",gtri)
  , ("gtrr",gtrr)
  , ("eqir",eqir)
  , ("eqri",eqri)
  , ("eqrr",eqrr) ]

opFn :: N a => String -> Op a
opFn name = case L.find ((name ==) . fst) ops of
  Just (_,fn) -> fn
  Nothing -> error "inexistent op name"


-- | Addition

-- add register
addr :: N a => Op a
addr a b c m = set m c $ get m a + get m b

-- add immediate
addi :: N a => Op a
addi a b c m = set m c $ get m a + b


-- | Multiplication

-- multiply register
mulr :: N a => Op a
mulr a b c m = set m c $ get m a * get m b

-- multiply immediate
muli :: N a => Op a
muli a b c m = set m c $ get m a * b


-- | Bitwise AND

-- bitwise AND register
banr :: N a => Op a
banr a b c m = set m c $ get m a .&. get m b

-- bitwise AND immediate
bani :: N a => Op a
bani a b c m = set m c $ get m a .&. b


-- | Bitwise OR

-- bitwise OR register
borr :: N a => Op a
borr a b c m = set m c $ get m a .|. get m b

-- bitwise OR immediate
bori :: N a => Op a
bori a b c m = set m c $ get m a .|. b


-- | Assignment

-- set register
setr :: N a => Op a
setr a _ c m@M{..} = set m c $ get m a

-- set immediate
seti :: N a => Op a
seti a _ c m@M{..} = set m c $ a


-- | Greater-than testing

-- greater-than immediate/register
gtir :: N a => Op a
gtir a b c m = set m c $ if a > get m b then 1 else 0

-- greater-than register/immediate
gtri :: N a => Op a
gtri a b c m = set m c $ if get m a > b then 1 else 0

-- greater-than register/register
gtrr :: N a => Op a
gtrr a b c m = set m c $ if get m a > get m b then 1 else 0


-- | Equality testing

-- equal immediate/register
eqir :: N a => Op a
eqir a b c m = set m c $ if a == get m b then 1 else 0

-- equal register/immediate
eqri :: N a => Op a
eqri a b c m = set m c $ if get m a == b then 1 else 0

-- equal register/register
eqrr :: N a => Op a
eqrr a b c m = set m c $ if get m a == get m b then 1 else 0
