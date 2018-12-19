{-# language ConstraintKinds #-}
{-# language RecordWildCards #-}

module Machine where

import Data.Int
import Data.Bits


-- | Machine (four registers: r0,r1,r2,r3)

data M a = M { r0 :: !a, r1 :: !a, r2 :: !a, r3 :: !a }
  deriving (Show,Eq)


-- | Machine number

type N a = (Num a, Bits a, Ord a, Read a, Show a)


-- | Construction

fromList :: N a => [a] -> M a
fromList [a,b,c,d] = M a b c d
fromList _ = error "wrong machine format"


-- | Primitives

set :: N a => M a -> a -> a -> M a
set _ i _ | i < 0 || i > 3 = error "setting non-existing register"
set m 0 x = m { r0 = x }
set m 1 x = m { r1 = x }
set m 2 x = m { r2 = x }
set m 3 x = m { r3 = x }

get :: N a => M a -> a -> a
get _ i | i < 0 || i > 3 = error "getting non-existing register"
get M{..} 0 = r0
get M{..} 1 = r1
get M{..} 2 = r2
get M{..} 3 = r3


-- | Operations

type Op a = M a -> a -> a -> a -> M a

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


-- | Addition

-- add register
addr :: N a => Op a
addr m@M{..} a b c = set m c $ get m a + get m b

-- add immediate
addi :: N a => Op a
addi m@M{..} a b c = set m c $ get m a + b


-- | Multiplication

-- multiply register
mulr :: N a => Op a
mulr m@M{..} a b c = set m c $ get m a * get m b

-- multiply immediate
muli :: N a => Op a
muli m@M{..} a b c = set m c $ get m a * b


-- | Bitwise AND

-- bitwise AND register
banr :: N a => Op a
banr m@M{..} a b c = set m c $ get m a .&. get m b

-- bitwise AND immediate
bani :: N a => Op a
bani m@M{..} a b c = set m c $ get m a .&. b


-- | Bitwise OR

-- bitwise OR register
borr :: N a => Op a
borr m@M{..} a b c = set m c $ get m a .|. get m b

-- bitwise OR immediate
bori :: N a => Op a
bori m@M{..} a b c = set m c $ get m a .|. b


-- | Assignment

-- set register
setr :: N a => Op a
setr m@M{..} a _ c = set m c $ get m a

-- set immediate
seti :: N a => Op a
seti m@M{..} a _ c = set m c $ a


-- | Greater-than testing

-- greater-than immediate/register
gtir :: N a => Op a
gtir m@M{..} a b c = set m c $ if a > get m b then 1 else 0

-- greater-than register/immediate
gtri :: N a => Op a
gtri m@M{..} a b c = set m c $ if get m a > b then 1 else 0

-- greater-than register/register
gtrr :: N a => Op a
gtrr m@M{..} a b c = set m c $ if get m a > get m b then 1 else 0


-- | Equality testing

-- equal immediate/register
eqir :: N a => Op a
eqir m@M{..} a b c = set m c $ if a == get m b then 1 else 0

-- equal register/immediate
eqri :: N a => Op a
eqri m@M{..} a b c = set m c $ if get m a == b then 1 else 0

-- equal register/register
eqrr :: N a => Op a
eqrr m@M{..} a b c = set m c $ if get m a == get m b then 1 else 0
