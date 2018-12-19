{-# language ConstraintKinds #-}
{-# language RecordWildCards #-}

module Machine where

import Data.Int
import Data.Bits


-- | Machine (four registers: r0,r1,r2,r3)

data M a = M { r0 :: !a, r1 :: !a, r2 :: !a, r3 :: !a }
  deriving (Show,Eq)


-- | Machine number

type N a = (Num a, Bits a, Ord a)


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


-- | Addition

-- add register
addr :: N a => M a -> a -> a -> a -> M a
addr m@M{..} a b c = set m c $ get m a + get m b

-- add immediate
addi :: N a => M a -> a -> a -> a -> M a
addi m@M{..} a b c = set m c $ get m a + b


-- | Multiplication

-- multiply register
mulr :: N a => M a -> a -> a -> a -> M a
mulr m@M{..} a b c = set m c $ get m a * get m b

-- multiply immediate
muli :: N a => M a -> a -> a -> a -> M a
muli m@M{..} a b c = set m c $ get m a * b


-- | Bitwise AND

-- bitwise AND register
banr :: N a => M a -> a -> a -> a -> M a
banr m@M{..} a b c = set m c $ get m a .&. get m b

-- bitwise AND immediate
bani :: N a => M a -> a -> a -> a -> M a
bani m@M{..} a b c = set m c $ get m a .&. b


-- | Bitwise OR

-- bitwise OR register
borr :: N a => M a -> a -> a -> a -> M a
borr m@M{..} a b c = set m c $ get m a .|. get m b

-- bitwise OR immediate
bori :: N a => M a -> a -> a -> a -> M a
bori m@M{..} a b c = set m c $ get m a .|. b


-- | Assignment

-- set register
setr :: N a => M a -> a -> a -> a -> M a
setr m@M{..} a _ c = set m c $ get m a

-- set immediate
seti :: N a => M a -> a -> a -> a -> M a
seti m@M{..} a _ c = set m c $ a


-- | Greater-than testing

-- greater-than immediate/register
gtir :: N a => M a -> a -> a -> a -> M a
gtir m@M{..} a b c = set m c $ if a > get m b then 1 else 0

-- greater-than register/immediate
gtri :: N a => M a -> a -> a -> a -> M a
gtri m@M{..} a b c = set m c $ if get m a > b then 1 else 0

-- greater-than register/register
gtrr :: N a => M a -> a -> a -> a -> M a
gtrr m@M{..} a b c = set m c $ if get m a > get m b then 1 else 0


-- | Equality testing

-- equal immediate/register
eqir :: N a => M a -> a -> a -> a -> M a
eqir m@M{..} a b c = set m c $ if a == get m b then 1 else 0

-- equal register/immediate
eqri :: N a => M a -> a -> a -> a -> M a
eqri m@M{..} a b c = set m c $ if get m a == b then 1 else 0

-- equal register/register
eqrr :: N a => M a -> a -> a -> a -> M a
eqrr m@M{..} a b c = set m c $ if get m a == get m b then 1 else 0
