module Main where

import Data.Ord
import Data.Char

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

type Graph = [Node]

mkGraph cs input = map (\c -> Node c $ map snd . filter ((c==) . fst) $ input) cs

(=-) :: Graph -> Node -> Graph
g =- n = concatMap (remove n) g

remove (Node c (_:_)) _ = error "removing connected node"
remove (Node c _) (Node c' _) | c == c' = []
remove (Node c _) (Node c' rs) = [Node c' (rs L.\\ [c])]

pop :: Graph -> (Node,Graph)
pop [] = error "empty graph"

pop g@[n@(Node c [])] = (n,[])
pop [Node c _] = error "edges without nodes"

pop g = (m,g =- m)
  where
    m = minimum g

crawl [] = []
crawl g = c : crawl g'
  where
    (n,g') = pop g
    c = name_ n
