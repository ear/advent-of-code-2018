{-# language RecordWildCards #-}
{-# language PatternSynonyms #-}
{-# language OverloadedLists #-}
{-# language ViewPatterns #-}
module Main where

import Debug.Trace

import Data.Ord
import Data.Set        ( Set )
import Data.Map.Strict ( Map )
import Data.Char
import Data.Maybe
import Data.Function

import qualified Data.Set         as S
import qualified Data.Map.Strict  as M
import qualified Data.List        as L

import Control.Arrow

--

type Step = Char

duration :: Step -> Int
duration x = ord x - ord 'A' + 1

--

data Node = Node
  { step_ :: Step
  , reqs_ :: Set Step
  } deriving (Eq, Show)

instance Ord Node where
  compare = comparing (length . reqs_) <> comparing step_

pattern Free s <- Node s []

removeReq :: Step -> Node -> Node
removeReq s n@Node{..} = n { reqs_ = S.delete s reqs_ }

--

type Graph = Set Node

fromList :: [(Step,Step)] -> Graph
fromList xs = g
  where
    ss = uncurry S.union
       . (S.fromList *** S.fromList)
       . unzip
       $ xs
    g = S.map (\s -> Node s $ requirements s) ss
    requirements s = S.fromList . map fst . filter ((s ==) . snd) $ xs

isEmpty :: Graph -> Bool
isEmpty = S.null

minView :: Graph -> Maybe (Node, Graph)
minView = S.minView

removeSteps :: Graph -> [Step] -> Graph
removeSteps = L.foldl' removeStep

removeStep :: Graph -> Step -> Graph
removeStep g s = S.map (removeReq s) . S.filter ((s /=) . step_) $ g

--

type ID = Int
type Job = (Step, Int)

data Worker = Worker
  { id_ :: Int
  , job_ :: Maybe Job
  }

pattern Idle   <- Worker _ Nothing
pattern Done s <- Worker _ (Just (s,0))
pattern Busy s <- Worker _ (Just (s,_))

jobStep :: Worker -> Step
jobStep (Busy s) = s
jobStep _ = error "jobStep of an idle worker"

instance Eq Worker where
  a == b = id_ a == id_ b

instance Ord Worker where
  compare = comparing id_

mkIdleWorker :: Int -> Worker
mkIdleWorker i = Worker
  { id_  = i
  , job_ = Nothing
  }

isIdle :: Worker -> Bool
isIdle (Worker _ Nothing) = True
isIdle _ = False

setIdle :: Worker -> Worker
setIdle w = w { job_ = Nothing }

isDone :: Worker -> Bool
isDone (Worker _ (Just (_,0))) = True
isDone _ = False

mkWorker :: Int -> Job -> Worker
mkWorker i j = Worker
  { id_  = i
  , job_ = Just j
  }

--

type Workers = Set Worker

mkWorkers :: Int -> Workers
mkWorkers n = S.fromList . map mkIdleWorker $ [1..n]

partitionWorkers :: Workers -> (Workers,Workers) -- (idle,busy)
partitionWorkers = S.partition isIdle

-- partitionWorkers :: Workers -> ([ID],[ID]) -- (idle,busy)
-- partitionWorkers = (map id_ *** map id_) . L.partition isIdle . S.elems

idle :: Workers -> [ID]
idle = map id_ . S.elems . fst . partitionWorkers

busy :: Workers -> [ID]
busy = map id_ . S.elems . snd . partitionWorkers

activeSteps :: Workers -> [Step]
activeSteps ws = [ s | Busy s <- S.elems ws ]

advance :: Workers -> Workers
advance = S.map decrease
  where
    decrease w@Idle = w
    decrease w@Worker{..} = w { job_ = fmap (second pred) job_ }

done :: Workers -> [Step]
done ws = [ s | Done s <- S.elems ws ]

--

data Work = Work
  { t_  :: Int
  , ws_ :: Workers
  , g_  :: Graph
  }

begin :: Int -> Graph -> Work
begin n g = Work
  { t_  = -1
  , ws_ = mkWorkers n
  , g_  = g
  }

dispatch :: Work -> Work
dispatch w@Work{..} = w'
  where
    associations = zip (idle ws_) (freeUnhandledSteps w)
    w' = w =++ associations

tick :: Work -> Maybe Work
tick w@Work{..} | isEmpty g_ && null (busy ws_) = Nothing
tick w@Work{..} = Just $ dispatch $ w
  { t_  = t'
  , ws_ = ws''
  , g_  = g'
  }
  where
    t' = succ t_

    ws' = advance ws_
    (finished,running) = S.partition isDone ws'
    ws'' = (S.map setIdle finished) `S.union` ws' -- free finished workers

    -- clear the graph of every reference to the finished steps
    g' = g_ `removeSteps` (S.elems $ S.map jobStep finished)

--

isUnhandled :: Work -> Step -> Bool
isUnhandled w i = i `notElem` activeSteps (ws_ w)

freeUnhandledSteps :: Work -> [Step]
freeUnhandledSteps work@Work{..} = [ s | Free s <- S.elems g_, isUnhandled work s ]

--

(=++) :: Work -> [(ID,Step)] -> Work
w =++ assocs = L.foldl' (=+) w assocs

(=+) :: Work -> (ID,Step) -> Work
work =+ (i,s) = work { ws_ = S.insert w (ws_ work) }
  where
    w = mkWorker i (s,duration s)

assemble :: Int -> Graph -> [Work]
assemble n = L.unfoldr (fmap (\w -> (w,w)) . tick) . begin n

--

parse :: String -> [(Step,Step)]
parse = pairs . pluck . concatMap tail . lines -- skip the first letter of each line
  where
    pluck [] = []
    pluck ( x :xs) | isUpper x = x : pluck xs
                   | otherwise =     pluck xs
    pairs [] = []
    pairs (a:b:xs) = (a,b) : pairs xs

--

--- Pretty-printing

instance Show Worker where
  showsPrec _ Worker{..} = showJob job_
    where
      showJob Nothing = showString "."
      showJob (Just (c,t)) = shows c . showChar ' ' . shows t

instance Show Work where
  showsPrec _ Work{..}
    = shows t_ . showChar '\t'
    . showString (L.intercalate "\t" (map show $ S.elems ws_)) . showChar '\t'
    . showGraph g_
    where
      showGraph = shows . map (\(Node c rs) -> (c,S.toList rs)) . S.toList

--

part1 :: Graph -> [Step]
part1 = L.unfoldr (fmap (first step_) . minView)

--

part2 :: Graph -> IO ()
part2 g = do
  let assembly = assemble 2 g
  mapM_ print assembly
  putStr "seconds to complete the assembly: "
  print . t_ . last $ assembly

--

main :: IO ()
main = do
  g <- fromList . parse <$> readFile "input.txt"
  putStrLn . part1 $ g
  g' <- fromList . parse <$> readFile "test.txt"
  part2 g'
