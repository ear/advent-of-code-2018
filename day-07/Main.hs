{-# language RecordWildCards #-}
{-# language PatternSynonyms #-}
{-# language OverloadedLists #-}
{-# language ViewPatterns #-}

module Main where

import Data.Ord  ( comparing )
import Data.Set  ( Set )
import Data.Char ( ord, isUpper )

import qualified Data.Set  as S
import qualified Data.List as L

import Control.Arrow ( first, second, (***) )

--

type Step = Char

duration :: Step -> Int
duration x = ord x - ord 'A' + 1 + 60

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
type Job = (Step, Int) -- (step, time left to complete)

data Worker = Worker
  { id_ :: ID
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

mkIdleWorkers :: Int -> Workers
mkIdleWorkers n = S.fromList . map mkIdleWorker $ [1..n]

partitionWorkers :: Workers -> (Workers,Workers) -- (idle,busy)
partitionWorkers = S.partition isIdle

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

areRunning :: Workers -> Step -> Bool
ws `areRunning` s = s `elem` activeSteps ws

--

data Work = Work
  { t_  :: Int
  , ws_ :: Workers
  , g_  :: Graph
  }

(=++) :: Work -> [(ID,Step)] -> Work
w =++ assocs = L.foldl' (=+) w assocs

(=+) :: Work -> (ID,Step) -> Work
work =+ (i,s) = work { ws_ = S.insert w (ws_ work) }
  where
    w = mkWorker i (s,duration s)

begin :: Int -> Graph -> Work
begin n g = Work
  { t_  = -1
  , ws_ = mkIdleWorkers n
  , g_  = g
  }

stepsToDo :: Work -> [Step]
stepsToDo Work{..} = [ s | Free s <- S.elems g_, not (ws_ `areRunning` s) ]

dispatch :: Work -> Work
dispatch w@Work{..} = w'
  where
    associations = zip (idle ws_) (stepsToDo w)
    w' = w =++ associations

tick :: Work -> Maybe Work
tick   Work{..} | isEmpty g_ && null (busy ws_) = Nothing
tick w@Work{..} = Just $ dispatch $ w
  { t_  = succ t_
  , ws_ = ws''
  , g_  = g'
  }
  where
    ws'      = advance ws_

    -- collect finished workers and set them Idle again
    finished = S.filter isDone ws'
    ws''     = (S.map setIdle finished) `S.union` ws'

    -- clear the graph of every reference to the finished steps
    g' = g_ `removeSteps` (S.elems $ S.map jobStep finished)

--

parse :: String -> [(Step,Step)]
parse = pairs . pluck . concatMap tail . lines -- skip the first letter of each line
  where
    pluck [] = []
    pluck ( x :xs) | isUpper x = x : pluck xs
                   | otherwise =     pluck xs
    pairs [] = []
    pairs (a:b:xs) = (a,b) : pairs xs

--- Pretty-printing

instance Show Worker where
  showsPrec _ Worker{..} = showsJob job_
    where
      showsJob Nothing      = showString "."
      showsJob (Just (c,t)) = showChar c . showChar ' ' . shows t

instance Show Work where
  showsPrec _ Work{..}
    = shows t_ . showChar '\t'
    . (foldr (.) id $ map (\c -> showChar '\t' . shows c) (S.elems ws_)) . showChar '\t'
    . showGraph g_
    where
      showGraph = foldr (.) id . map (\n -> showNode n . showChar ' ') . map (\(Node c rs) -> (c,S.toList rs)) . S.toList
      showNode (c,[]) = showChar '[' . showChar c . showChar ']'
      showNode (c,rs) = showChar c . showString "->" . showString rs

-- Part 1

part1 :: Graph -> [Step]
part1 = L.unfoldr (fmap (first step_) . minView)

-- Part 2

assemble :: Int -> Graph -> [Work]
assemble n = L.unfoldr (fmap (\w -> (w,w)) . tick) . begin n

part2 :: Graph -> IO ()
part2 g = do
  let assembly = assemble 6 g
  mapM_ print assembly
  putStr "seconds to complete the assembly: "
  print . t_ . last $ assembly

-- Main

main :: IO ()
main = do
  g <- fromList . parse <$> readFile "input.txt"
  putStrLn . part1 $ g
  -- g' <- fromList . parse <$> readFile "test.txt"
  part2 g
