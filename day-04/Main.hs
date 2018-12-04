{-# language ViewPatterns #-}
{-# language NamedFieldPuns #-}
module Main where

import Data.Ord
import Data.List

main = do
  input <- lines <$> readFile "input.txt"
  let entries = computeIDs . sortBy entrySort . parse $ input
  mapM_ print entries

data Sleep = Wake | Fall
  deriving (Show)

data Time = Time
  { _y  :: Int   -- Year
  , _m  :: Int   -- Month
  , _d  :: Int   -- Day
  , _hh :: Int   -- Hour
  , _mm :: Int   -- Minute
  } deriving (Show)

data Entry = Entry
  { _t :: Time  -- yyyy-mm-dd HH:MM
  , _i :: Int   -- ID
  , _s :: Sleep -- Wake/Fall
  } deriving (Show)

-- parse :: [String] -> [Entry]
parse = map (parseLine . words . clean)
  where
    clean :: String -> String
    clean [] = []
    clean (' ':'f':'a':'l':'l':'s':_) = " F"
    clean (' ':'w':'a':'k':'e':'s':_) = " W"
    clean (c:cs) = (if c `elem` "0123456789FW" then c else ' ') : clean cs

    toEntry :: String -> Entry
    toEntry = parseLine . words

    parseLine :: [String] -> Entry
    parseLine
      [ read -> y, read -> m, read -> d, read -> hh, read -> mm, meta ] =
      case parseMeta meta of
        Left i  -> Entry (Time y m d hh mm)   i  Wake
        Right s -> Entry (Time y m d hh mm) (-1) s

    parseMeta "F" = Right Fall
    parseMeta "W" = Right Wake
    parseMeta xs  = Left $ read xs

entrySort :: Entry -> Entry -> Ordering
entrySort (Entry (Time ly lm ld lhh lmm) _ _)
          (Entry (Time ry rm rd rhh rmm) _ _)
  = compare ly ry <> compare lm rm <> compare ld rd <>
    compare lhh rhh <> compare lmm rmm

computeIDs :: [Entry] -> [Entry]
computeIDs (e:es) = map fst $ scanl' walk (e,_i e) es
  where
    walk (prevE,prevID) e
      | _i e == -1 = (e { _i = prevID }, prevID)
      | otherwise  = (e, _i e)

