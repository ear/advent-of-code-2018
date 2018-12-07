{-# language RecordWildCards #-}
{-# language PatternSynonyms #-}
module Main where

import Debug.Trace

import Data.Ord
import Data.Map  ( Map )
import Data.Char
import Data.Maybe

import qualified Data.Map  as M
import qualified Data.List as L

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input.txt"
  -- all the letters
  let cs = L.nub . L.sort . concatMap (\(a,b) -> [a,b]) $ input
  -- nodes
  let g = mkGraph cs input
  -- solution
  putStrLn $ crawl g
  -- part 2
  let steps = construct 6 g
  --mapM_ print steps
  print $ length steps

-- Parsing

type Dep = (Char,Char)

parse :: String -> Dep
parse = (\[a,b]->(head b,head a)) . words . map clean . tail
  where
    clean c | isUpper c = c | otherwise = ' '

-- Nodes

data Node = Node
  { name_ :: Char
  , reqs_ :: [Char]
  } deriving (Eq, Show)

instance Ord Node where
  compare = compareNodes

compareNodes :: Node -> Node -> Ordering
compareNodes = comparing (length . reqs_) <> comparing name_

-- Graph

newtype Graph = Graph [Node]
  deriving (Eq, Ord)

mkGraph cs input = Graph $ map (\c -> Node c $ map snd . filter ((c==) . fst) $ input) cs

(=-) :: Graph -> Node -> Graph
(Graph g) =- n = Graph $ concatMap (remove n) g

remove (Node c (_:_)) _ = error "removing connected node"
remove (Node c _) (Node c' _) | c == c' = []
remove (Node c _) (Node c' rs) = [Node c' (rs L.\\ [c])]

pop :: Graph -> (Node,Graph)
pop (Graph []) = error "empty graph"

pop g@(Graph [n@(Node c [])]) = (n,Graph [])
pop (Graph [Node c _]) = error "edges without nodes"

pop g@(Graph ns) = (m,g =- m)
  where
    m = minimum ns

crawl (Graph []) = []
crawl g = c : crawl g'
  where
    (n,g') = pop g
    c = name_ n

empty :: Graph -> Bool
empty (Graph []) = True
empty _ = False

freeNodes :: Graph -> Maybe [Node]
freeNodes (Graph ns) | null ns = Nothing
freeNodes (Graph ns)
  = case L.filter (null . reqs_) ns of
      [] -> Nothing
      fs -> Just fs

-- Parallel workers

data Worker = Worker
  { id_ :: Int
  , job_ :: Maybe (Char, Int) -- job char and job time left
  }

pattern Idle <- Worker _ Nothing
pattern Working c t <- Worker _ (Just (c,t))

data Work = Work
  { t_  :: Int
  , ws_ :: Map Int Worker
  , g_  :: Graph
  }

start :: Int -> Graph -> Work
start n = Work (0) (M.fromList $ zip [1..] $ map (\i -> Worker i Nothing) [1..n])

workerDone = isNothing . job_

workersDone :: Work -> Bool
workersDone Work{..} = all workerDone ws_

tick :: Work -> Maybe Work
tick w@Work{..} | workersDone w && empty g_ = Nothing
tick w@Work{..}
  = case freeWorkers w of
      [] -> Just w'
      ws -> case availableSteps w of
              Nothing -> Just w'
              Just cs -> Just w''
                where
                  --assignments = traceShow ("assignments:", zip ws cs) $ zip ws cs
                  assignments = zip ws cs
                  w'' = stepWorkers $ assignWork w assignments g_
  where
    w' = stepWorkers w

freeWorkers :: Work -> [Int]
freeWorkers Work{..} = M.keys . M.filter (isNothing . job_) $ ws_

currentJobs :: Work -> [Char]
currentJobs Work{..} = catMaybes . map (fmap fst . job_) . M.elems $ ws_

availableSteps :: Work -> Maybe [Char]
availableSteps w@Work{..}
  = (filter (`notElem` jobs) . map name_) <$> freeNodes g_
    where
      jobs = currentJobs w

-- only called if there are both freeWorkers and availableStep
assignWork :: Work -> [(Int,Char)] -> Graph -> Work
assignWork Work{..} pairs g' = Work t_ ws' g'
  where
    ws' = L.foldl' assign ws_ pairs
    -- m = workers map; i = worker id; c = job char
    assign m (i,c) = M.insert i (Worker i (Just (c,ord c - ord 'A' + 2 + 60))) m

stepWorkers :: Work -> Work
stepWorkers Work{..} = w'
  where
    w' = Work t' ws' g'
    t' = succ t_
    (freed,ws') = M.mapAccum advance [] ws_

    advance freed w@Idle = (freed,w)
    advance freed w@(Working c t)
      = case pred t of
          1 -> (c : freed, w { job_ = Nothing    })
          n -> (    freed, w { job_ = Just (c,n) })

    g' = L.foldl' (=-) g_ $ map (\c -> Node c []) freed

construct :: Int -> Graph -> [Work]
construct n = L.unfoldr (fmap (\x->(x,x)) . tick) . start n

-- Pretty-printing

instance Show Worker where
  showsPrec _ Worker{..} = showJob job_
    where
      showJob Nothing = showString "."
      showJob (Just (c,t)) = shows c . showChar ' ' . shows t

instance Show Work where
  showsPrec _ Work{..}
    = shows t_ . showChar '\t'
    . showString (L.intercalate "\t" (map show $ M.elems ws_)) . showChar '\t'
    . shows g_

instance Show Graph where
  showsPrec _ (Graph ns) = shows . map (\(Node c rs) -> (c,rs)) $ ns
