{-# language RecordWildCards #-}

module Main where

import Data.Array.Unboxed ( UArray, (!), (//) )
import qualified Data.Array.Unboxed as A

data T = T
  { s_  :: UArray Int Int -- ^ scores
  , n_  :: Int            -- ^ length of the filled prefix2000000
  , e1_ :: Int            -- ^ elf 1 index
  , e2_ :: Int            -- ^ elf 2 index
  } deriving Show

begin :: T
begin = T (A.listArray (0,30) [3,7]) 2 0 1

-- | One step of recipe making and assignment to the scoreboard
--
tick :: T -> T
tick T{..} = T s' n' e1' e2'
  where
    d1 = s_ ! e1_
    d2 = s_ ! e2_
    sum = d1+d2
    ds = digits sum
    n' | sum < 10  = n_ + 1
       | otherwise = n_ + 2 -- sums are in [0,18]
    s' = s_ // (zip [n_..] $ digits sum)
    e1' = (e1_ + succ d1) `mod` n'
    e2' = (e2_ + succ d2) `mod` n'

p :: T -> String
p T{..} = line
  where
    scores = take n_ $ A.elems s_
    line = (snd $ foldl go (0,id) scores) ""
    go (n,rest) s
      = ( succ n
        , case n of
            _ | n == e1_ -> rest . showChar '(' . shows s . showChar ')'
            _ | n == e2_ -> rest . showChar '[' . shows s . showChar ']'
            _            -> rest . showChar ' ' . shows s . showChar ' '
        )

-- | Break a number into digits
--
-- >>> digits 10
-- [1,0]
-- >>> digits 1234567
-- [1,2,3,4,5,6,7]
--
-- TODO: use divMod
digits :: Int -> [Int]
digits n = reverse $ take len ds
  where
    len | n < 10 = 1
        | n == 10 = 2
        | otherwise = ceiling (log (fromIntegral n) / log 10)
    ds = map (`mod` 10) $ iterate pop $ n
    pop x = truncate $ fromIntegral x / 10

input = 846601

main = do
  print ()
