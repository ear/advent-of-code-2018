{-# language ConstraintKinds #-}
{-# language RecordWildCards #-}

module Machine where

import Data.Int


-- | Machine (four registers: r0,r1,r2,r3)

data M a = M { r0 :: !a, r1 :: !a, r2 :: !a, r3 :: !a }
  deriving (Show,Eq)


-- | Machine number

type N a = (Num a, Ord a)


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
addr m@M{..} a b c = set m c $ undefined

-- add immediate
addi :: N a => M a -> a -> a -> a -> M a
addi m@M{..} a b c = set m c $ undefined


-- | Multiplication

-- multiply register
mulr :: N a => M a -> a -> a -> a -> M a
mulr m@M{..} a b c = set m c $ undefined

-- multiply immediate
muli :: N a => M a -> a -> a -> a -> M a
muli m@M{..} a b c = set m c $ undefined


-- | Bitwise AND

-- bitwise AND register
banr :: N a => M a -> a -> a -> a -> M a
banr m@M{..} a b c = set m c $ undefined

-- bitwise AND immediate
bani :: N a => M a -> a -> a -> a -> M a
bani m@M{..} a b c = set m c $ undefined


-- | Bitwise OR

-- bitwise OR register
borr :: N a => M a -> a -> a -> a -> M a
borr m@M{..} a b c = set m c $ undefined

-- bitwise OR immediate
bori :: N a => M a -> a -> a -> a -> M a
bori m@M{..} a b c = set m c $ undefined


-- | Assignment

-- set register
setr :: N a => M a -> a -> a -> a -> M a
setr m@M{..} a b c = set m c $ undefined

-- set immediate
seti :: N a => M a -> a -> a -> a -> M a
seti m@M{..} a b c = set m c $ undefined


-- | Greater-than testing

-- greater-than immediate/register
gtir :: N a => M a -> a -> a -> a -> M a
gtir m@M{..} a b c = set m c $ undefined

-- greater-than register/immediate
gtri :: N a => M a -> a -> a -> a -> M a
gtri m@M{..} a b c = set m c $ undefined

-- greater-than register/register
gtrr :: N a => M a -> a -> a -> a -> M a
gtrr m@M{..} a b c = set m c $ undefined


-- | Equality testing

-- equal immediate/register
eqir :: N a => M a -> a -> a -> a -> M a
eqir m@M{..} a b c = set m c $ undefined

-- equal register/immediate
eqri :: N a => M a -> a -> a -> a -> M a
eqri m@M{..} a b c = set m c $ undefined

-- equal register/register
eqrr :: N a => M a -> a -> a -> a -> M a
eqrr m@M{..} a b c = set m c $ undefined
