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
  input <- map parse . lines <$> readFile "test.txt"
  -- all the letters
  let cs = L.nub . L.sort . concatMap (\(a,b) -> [a,b]) $ input
  -- nodes
  let g = mkGraph cs input
  -- solution
  putStrLn $ crawl g
  -- part 2
  print $ start g
  print . tick $ start g

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

freeNodes :: Graph -> Maybe ([Node],Graph)
freeNodes (Graph ns) | null ns = Nothing
freeNodes (Graph ns)
  = case L.partition (null . reqs_) ns of
      ([],_  ) -> Nothing
      (fs,ns') -> Just (fs, g')
        where
          g' = L.foldl' (=-) (Graph ns') fs

freeNodesExcept :: [Char] -> Graph -> Maybe ([Node],Graph)
freeNodesExcept _  (Graph ns) | null ns = Nothing
freeNodesExcept [] (Graph ns) = freeNodes (Graph ns)
freeNodesExcept cs (Graph ns)
  = case L.partition test ns of
      ([],_  ) -> Nothing
      (fs,ns') -> Just (fs, g')
        where
          g' = L.foldl' (=-) (Graph ns') fs
    where
      -- test: available (no requirements) and unassigned (not in the exception list)
      test Node{..} = null reqs_ && name_ `notElem` cs

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

start :: Graph -> Work
start = Work (-1) (M.fromList $ zip [1..] $ map (\i -> Worker i Nothing) [1..6])

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
              Just (cs,g') -> Just $ stepWorkers (assignWork w (zip ws cs) g')
  where
    w' = stepWorkers w

freeWorkers :: Work -> [Int]
freeWorkers Work{..} = M.keys . M.filter (isNothing . job_) $ ws_

availableSteps :: Work -> Maybe ([Char],Graph)
availableSteps Work{..}
  = (\(ns,g) -> (map name_ ns, g)) <$> freeNodesExcept (traceShowId currentlyWorkedOn) g_
    where
      currentlyWorkedOn = catMaybes . M.elems . fmap ((fmap fst) . job_) $ ws_

-- only called if there are both freeWorkers and availableStep
assignWork :: Work -> [(Int,Char)] -> Graph -> Work
assignWork Work{..} pairs g' = Work t_ ws' g'
  where
    ws' = L.foldl' assign ws_ pairs
    -- m = workers map; i = worker id; c = job char
    assign m (i,c) = M.insert i (Worker i (Just (c,ord c - ord 'A' + 2))) m

stepWorkers :: Work -> Work
stepWorkers Work{..} = w'
  where
    w' = Work t' ws' g_
    t' = succ t_
    ws' = fmap advance ws_

    advance w@Idle = w
    advance w@(Working c t)
      = case pred t of
          0 -> w { job_ = Nothing }
          n -> w { job_ = Just (c,n) }

construct :: Graph -> [Work]
construct = L.unfoldr (fmap (\x->(x,x)) . tick) . start

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
