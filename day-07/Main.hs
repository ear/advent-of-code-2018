module Main where

import Data.Ord
import Data.Set ( Set )
import Data.Char
import Data.Graph

import qualified Data.Set  as S
import qualified Data.List as L

main :: IO ()
main = do
  input <- map parse . lines <$> readFile "input.txt"
  -- all the letters
  let cs = L.nub . L.sort . concatMap (\(a,b) -> [a,b]) $ input
  putStrLn cs
  -- nodes
  let ns = nodes cs input
  mapM_ print ns
  let (g,fromV,fromK) = graphFromEdges ns
  print $ outdegree g
  let sorted = reverse . topSort $ g
  print $ map ((\(_,k,_) -> k) . fromV) sorted

type Dep = (Char,Char)

parse :: String -> Dep
parse = (\[a,b]->(head b,head a)) . words . map clean . tail
  where
    clean c | isUpper c = c | otherwise = ' '

nodes cs input = map (\c -> (c, c, map snd . filter ((c==) . fst) $ input)) cs
