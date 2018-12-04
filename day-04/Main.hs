{-# language ViewPatterns #-}
{-# language RecordWildCards #-}
{-# language PatternSynonyms #-}
module Main (main) where

import Debug.Trace

import Data.Ord
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

main = do
  input <- lines <$> readFile "input.txt"
  let entries = computeIDs . sortBy entrySort . parse $ input
  mapM_ print entries
  --let guards = aggregateByID entries
  let sleeps = stats entries
  Map.traverseWithKey (\k v -> print (k,length v)) sleeps
  let sleepiest = maximumBy (comparing (length . snd)) $ Map.toList sleeps
  let (gid,mins) = sleepiest
  putStr "sleepiest guard (id,mins): "
  print (gid, length mins)
  putStr "sleepiest minute (minute,count): "
  let r = maximumBy (comparing snd)
        . map (\n -> (head n, length n)) . group . sort $ mins
  print r
  putStr "product: "
  print $ gid * fst r
  let guards = byMinute sleeps
  let (gid,min,_) = mostAsleepOnSameMin guards
  putStr "sleepiest' guard (gid,minute): "
  print (gid,min)
  putStr "product': "
  print $ gid * min

data Sleep = Wake | Fall
  deriving (Show)

data Time = Time
  { _y  :: Int   -- Year
  , _m  :: Int   -- Month
  , _d  :: Int   -- Day
  , _hh :: Int   -- Hour
  , _mm :: Int   -- Minute
  } deriving (Ord, Eq)

data Day = Day
  { _dy :: Int
  , _dm :: Int
  , _dd :: Int
  } deriving (Ord, Eq)

toDay :: Time -> Day
toDay (Time {..}) = Day _y _m _d

instance Show Time where
  showsPrec _ (Time {..}) =
    --showsPrec 9 _y   . showChar '-' .
    showsPad 2 9 _m  . showChar '-' .
    showsPad 2 9 _d  . showChar ' ' .
    showsPad 2 9 _hh . showChar ':' .
    showsPad 2 9 _mm

instance Show Day where
  showsPrec _ (Day {..}) = showsPad 2 9 _dm . showChar '-' . showsPad 2 9 _dd

showsPad len p x = showString (replicate n '0') . showsPrec p x
  where
    digits = length $ show x
    n | len > digits = len - digits
      | otherwise    = 0

data Entry = Entry
  { _t :: Time  -- yyyy-mm-dd HH:MM
  , _i :: Int   -- ID
  , _s :: Sleep -- Wake/Fall
  }

instance Show Entry where
  showsPrec _ (Entry {..}) =
    showString "<" .
    showChar '"' . showsPrec 9 _t . showString "\" " .
    showsPrec 9 _i . showString " " .
    showsPrec 9 _s . showChar '>'

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

aggregateByID :: [Entry] -> Map (Int) [Entry]
aggregateByID = foldl' agg Map.empty
  where
    agg m e@(Entry {..}) = Map.alter (add e) (_i) m
    add e Nothing = Just [e]
    add e (Just es) = Just (e:es)

pattern Awake  t i = Entry t i Wake
pattern Asleep t i = Entry t i Fall

-- compute list of minutes asleep for each guard id
-- e.g. (10,[58,59,00,01,20,21,22])
stats :: [Entry] -> Map Int [Int]
stats (e:es) = fst $ foldl' stopwatch (Map.empty,e) es
  where
    stopwatch (m,prev@(Awake  _ pi)) next@(Awake  _ i)
      | pi /= i   = (m,next) -- guard change
      | otherwise = (m,prev) -- keep the earliest awake time for a guard

    stopwatch (m,prev@(Awake  _ pi)) next@(Asleep _ i)
      | pi /= i   = error "wrong guard fell asleep"
      | otherwise = (m,next) -- remember this fall asleep time

    stopwatch (m,prev@(Asleep _ pi)) next@(Asleep _ i)
      | pi /= i   = error "wrong guard keeps sleeping"
      | otherwise = error "same guard sleeping twice?"

    stopwatch (m,prev@(Asleep pt pi)) next@(Awake t i)
      | pi /= i   = error "wrong guard woke up"
      | otherwise = (track m i (timeSpan pt t), next) -- save sleeping mins

    track m i mins = Map.alter (collect mins) i m
      where
        collect mins Nothing     = Just $ mins
        collect mins (Just prev) = Just $ prev ++ mins

timeSpan :: Time -> Time -> [Int]
timeSpan l@(Time ly lm ld lhh lmm) r@(Time ry rm rd rhh rmm) =
  case rd-ld of
    0 -> [lmm..rmm-1]
    1 -> [lmm..59] ++ [0..rmm-1]

-- from: guard id -> [minutes asleep]
--   to:   minute -> [guard ids asleep]
byMinute :: Map Int [Int] -> Map Int [Int]
byMinute
  = Map.fromListWith (++)
  . map (\t -> (snd t, [fst t]))
  . concatMap (\(gid,mins) -> map (\min -> (gid,min)) mins)
  . Map.toList

-- returns: (guard id, max minute, minute's count)
mostAsleepOnSameMin :: Map Int [Int] -> (Int,Int,Int)
mostAsleepOnSameMin = Map.foldlWithKey' walk (-1,-1,0)
  where
    -- (i,m,c) = (guard id, max minute, minute's count)
    -- m' = new minute in cosideration
    walk (i,m,c) m' ids = ans
      where
        freqs = map (\xs -> (head xs, length xs)) . group . sort $ ids
        -- i' = new guard id
        -- c' = new minute count
        (i',c') = maximumBy (comparing snd) freqs
        ans | c' > c = (i',m',c')
            | otherwise = (i,m,c)
